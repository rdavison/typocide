open! Import

type t =
  { dim : Dim.t
  ; cursor : Cursor.t
  ; text : Text.t
  ; problem_words : int String.Map.t
  ; prev_words : int String.Map.t String.Map.t
  ; next_words : int String.Map.t String.Map.t
  ; triples : int String.Map.t String.Map.t
  ; mode : [ `Main | `Practice of string list ]
  ; bigram_times : Time_float.Span.t String.Map.t
  ; word_times : Time_float.Span.t String.Map.t
  ; corpus : Corpus.t
  }
[@@deriving sexp]

let version = 0

type model = t [@@deriving sexp]

module V0 = struct
  type nonrec t = t =
    { dim : Dim.t
    ; cursor : Cursor.t
    ; text : Text.t
    ; problem_words : int String.Map.t
    ; prev_words : int String.Map.t String.Map.t
    ; next_words : int String.Map.t String.Map.t
    ; triples : int String.Map.t String.Map.t
    ; mode : [ `Main | `Practice of string list ]
    ; bigram_times : Time_float.Span.t String.Map.t
    ; word_times : Time_float.Span.t String.Map.t
    ; corpus : Corpus.t
    }
  [@@deriving sexp]

  let upgrade (t : t) : model = t
end

let save_state t ~state_dir =
  match state_dir with
  | None -> ()
  | Some dir ->
    Core_unix.mkdir_p dir;
    let path = dir ^/ sprintf "state.%d.sexp" version in
    Stdio.Out_channel.write_all path ~data:(sexp_of_t t |> Sexp.to_string)
;;

let restore_state ~state_dir : t option =
  match state_dir with
  | None -> None
  | Some dir ->
    let version =
      Sys_unix.ls_dir dir
      |> List.filter_map ~f:(fun item ->
        match item |> String.strip |> String.split_on_chars ~on:[ '.' ] with
        | [ "state"; version; "sexp" ] -> Some (Int.of_string version)
        | _ -> None)
      |> List.sort ~compare:(Comparable.reverse Int.compare)
      |> List.hd
    in
    (match version with
     | None -> None
     | Some version ->
       let basename = sprintf "state.%d.sexp" version in
       if Sys_unix.file_exists_exn basename
       then (
         let parse s =
           match version with
           | 0 -> s |> V0.t_of_sexp |> V0.upgrade
           | _ -> failwithf "Unable to parse state file with verson: %d" version ()
         in
         Some (Core.In_channel.read_all basename |> Sexp.of_string |> parse))
       else None)
;;

let make_text text ~(dim : Dim.t) : Text.t =
  let cols, rows = Dim.cols_rows dim in
  let acc, line, _count =
    text
    |> String.split ~on:' '
    |> List.fold ~init:([], [], 0) ~f:(fun (acc, line, cumul) word ->
      let len = String.length word + 1 in
      if cumul + len < cols
      then acc, word :: line, cumul + len
      else List.rev line :: acc, [ word ], len)
  in
  let raw = List.rev (List.rev line :: acc) in
  List.mapi raw ~f:(fun row line ->
    let seen = ref 0 in
    List.mapi line ~f:(fun col word ->
      let line_offset = !seen in
      seen := line_offset + String.length word + 1;
      (col, row, line_offset), word))
  |> List.concat
  |> List.mapi ~f:(fun id ((col, row, line_offset), word) ->
    { Text.Word.id
    ; col
    ; row
    ; line_offset
    ; data = word
    ; typed = ""
    ; log = []
    ; state = `Pending
    })
;;

let set_cursor t cursor = { t with cursor }

let remake_text (text : Text.t) ~dim =
  let cols, rows = Dim.cols_rows dim in
  let acc, line, _count =
    text
    |> List.fold ~init:([], [], 0) ~f:(fun (acc, line, cumul) word ->
      let len = String.length word.data + 1 in
      if cumul + len < cols
      then acc, word :: line, cumul + len
      else List.rev line :: acc, [ word ], len)
  in
  let raw = List.rev (List.rev line :: acc) in
  List.mapi raw ~f:(fun row line ->
    let seen = ref 0 in
    List.mapi line ~f:(fun col word ->
      let line_offset = !seen in
      seen := line_offset + String.length word.data + 1;
      { word with col; row; line_offset }))
  |> List.concat
;;

let sanitize_text text =
  text
  |> String.split ~on:' '
  |> List.filter_map ~f:(fun s ->
    match String.strip s with
    | "" -> None
    | other -> Some other)
  |> String.concat ~sep:" "
;;

let set_dim t dim = { t with dim; text = remake_text t.text ~dim }

let create
  ~dim
  ~cursor
  ~text
  ~mode
  ~prev_words
  ~next_words
  ~triples
  ~bigram_times
  ~word_times
  ~corpus
  =
  let text =
    sanitize_text text
    |> make_text ~dim
    |> List.map ~f:(fun word -> if word.id = 0 then { word with state = `New } else word)
  in
  let problem_words = Map.empty (module String) in
  { dim
  ; cursor
  ; text
  ; mode
  ; problem_words
  ; prev_words
  ; next_words
  ; triples
  ; bigram_times
  ; word_times
  ; corpus
  }
;;

let make_practice_text t word =
  let practice_len = 10 in
  let triples =
    Map.find_exn t.triples word
    |> Map.to_alist
    |> List.fold ~init:[] ~f:(fun acc (triple, count) ->
      List.init count ~f:(Fn.const triple) @ acc)
    |> Array.of_list
  in
  let triples_len = Array.length triples in
  let triples =
    List.init (practice_len * 3) ~f:(fun _ -> triples.(Random.int triples_len))
  in
  let all = Array.of_list triples in
  let len = Array.length all in
  List.init practice_len ~f:(fun _ -> all.(Random.int len)) |> String.concat ~sep:" "
;;

let update_triples key wide_focus triples =
  triples
    := Map.update !triples key ~f:(function
         | None -> Map.singleton (module String) wide_focus 1
         | Some map ->
           Map.update map wide_focus ~f:(function
             | None -> 1
             | Some n -> n + 1))
;;

let update_worst ratio key worst =
  let w_ratio, w_word = !worst in
  if Float.( > ) ratio w_ratio then worst := ratio, key
;;

let variation_window len = 10. *. Float.exp (Float.of_int len *. -1.)

let process_word_time key start_time end_time word_times problem_words triples wide_focus =
  let word_span = Time_float.diff end_time start_time in
  let len = String.length key in
  word_times
    := Map.update !word_times key ~f:(function
         | None -> word_span
         | Some prev_avg ->
           let next_avg =
             Time_float.Span.(of_sec ((to_sec prev_avg +. to_sec word_span) /. 2.0))
           in
           let ratio = Time_float.Span.(to_sec next_avg /. to_sec prev_avg) in
           let window = variation_window len in
           if Float.( > ) ratio (window +. 1.)
           then (
             problem_words
               := Map.update !problem_words key ~f:(function
                    | None -> 1
                    | Some n -> n + 1);
             update_triples key wide_focus triples);
           next_avg)
;;

let process_bigram_time key start_time end_time bigram_times =
  let span = Time_float.diff end_time start_time in
  bigram_times
    := Map.update !bigram_times key ~f:(function
         | None -> span
         | Some span' -> Time_float.Span.(of_sec ((to_sec span' +. to_sec span) /. 2.0)))
;;

let update_problems t =
  let arr = Array.of_list t.text in
  let len = Array.length arr in
  let problem_words = ref t.problem_words in
  let triples = ref t.triples in
  let bigram_times = ref t.bigram_times in
  let word_times = ref t.word_times in
  for i = 0 to len - 1 do
    let prev = i - 1 in
    let next = i + 1 in
    let focus = arr.(i) in
    let wide_focus =
      [ (if prev >= 0 then Some arr.(prev) else None)
      ; Some focus
      ; (if next < Array.length arr then Some arr.(next) else None)
      ]
      |> List.filter_opt
      |> List.map ~f:(fun word -> word.data)
      |> String.concat ~sep:" "
    in
    if not (String.is_empty focus.typed)
    then
      if String.equal focus.data focus.typed
      then (
        let log =
          focus.log
          |> List.sort ~compare:(fun x y -> Time_float.compare x.time y.time)
          |> Array.of_list
        in
        let len = Array.length log in
        for i = 0 to len - 2 do
          let start_time = log.(i).time in
          if i = 0
          then (
            let end_time = log.(len - 1).time in
            let key = focus.data in
            process_word_time
              key
              start_time
              end_time
              word_times
              problem_words
              triples
              wide_focus);
          let j = i + 1 in
          let end_time = log.(j).time in
          let key = log.(i).keycode ^ log.(j).keycode in
          process_bigram_time key start_time end_time bigram_times
        done)
      else (
        let key = focus.data in
        problem_words
          := Map.update !problem_words key ~f:(function
               | None -> 1
               | Some n -> n + 1);
        update_triples key wide_focus triples)
  done;
  { t with
    problem_words = !problem_words
  ; triples = !triples
  ; bigram_times = !bigram_times
  ; word_times = !word_times
  }
;;

let should_repeat t =
  let correct, total =
    List.fold t.text ~init:(0, 0) ~f:(fun (correct, total) word ->
      (correct + if String.equal word.data word.typed then 1 else 0), total + 1)
  in
  let correct = Float.of_int correct in
  let total = Float.of_int total in
  let accuracy = correct /. total in
  Float.( < ) accuracy 0.93
;;

let current_word t =
  let id, _offset = Cursor.id_offset t.cursor in
  List.find_exn t.text ~f:(fun word -> word.id = id)
;;

let restart_game t =
  let cursor = Cursor.make (0, 0) in
  let text =
    List.map t.text ~f:(fun word ->
      let state = if word.id = 0 then `New else `Pending in
      let typed = "" in
      { word with state; typed })
  in
  { t with cursor; text }
;;

let process_tab t =
  let current_word = current_word t in
  let mode, text, t =
    match t.mode, current_word.id, current_word.state with
    | (`Main | `Practice []), 0, `New -> `Main, Corpus.next t.corpus, t
    | `Practice (next_text :: rest), 0, `New ->
      `Practice rest, make_practice_text t next_text, t
    | _, _, _ ->
      let t = restart_game t in
      let text = List.map t.text ~f:(fun word -> word.data) |> String.concat ~sep:" " in
      t.mode, text, t
  in
  create
    ~dim:t.dim
    ~cursor:(Cursor.make (0, 0))
    ~text
    ~mode
    ~prev_words:t.prev_words
    ~next_words:t.next_words
    ~triples:t.triples
    ~bigram_times:t.bigram_times
    ~word_times:t.word_times
    ~corpus:t.corpus
;;

let process_endgame t =
  let mode, text, t =
    match t.mode with
    | `Main ->
      let t = update_problems t in
      let problem_words = Map.to_alist t.problem_words |> List.map ~f:fst in
      (match problem_words with
       | fst :: rest -> `Practice rest, make_practice_text t fst, t
       | [] -> `Main, Corpus.next t.corpus, t)
    | `Practice [] ->
      if should_repeat t
      then
        ( `Practice []
        , List.map t.text ~f:(fun word -> word.data) |> String.concat ~sep:" "
        , t )
      else `Main, Corpus.next t.corpus, t
    | `Practice (word :: rest) ->
      if should_repeat t
      then
        ( `Practice (word :: rest)
        , List.map t.text ~f:(fun word -> word.data) |> String.concat ~sep:" "
        , t )
      else `Practice rest, make_practice_text t word, t
  in
  create
    ~dim:t.dim
    ~cursor:(Cursor.make (0, 0))
    ~text
    ~mode
    ~prev_words:t.prev_words
    ~next_words:t.next_words
    ~triples:t.triples
    ~bigram_times:t.bigram_times
    ~word_times:t.word_times
    ~corpus:t.corpus
;;

let handle_keypress t c =
  let id, offset = Cursor.id_offset t.cursor in
  match c with
  | ' ' ->
    let t = { t with cursor = Cursor.make (id + 1, 0) } in
    let id', offset' = Cursor.id_offset t.cursor in
    let text =
      List.map t.text ~f:(fun word ->
        if word.id = id
        then (
          let state = if String.equal word.data word.typed then `Success else `Failure in
          { word with state; log = List.rev word.log })
        else if word.id = id'
        then { word with state = `Active }
        else word)
    in
    let t = { t with text } in
    if id' >= List.length t.text then process_endgame t else t
  | _ ->
    let s = String.of_char c in
    let text =
      List.map t.text ~f:(fun word ->
        if word.id = id
        then
          { word with
            typed = word.typed ^ s
          ; state = `Active
          ; log = Text.Keylog.make s :: word.log
          }
        else word)
    in
    let t = { t with text } in
    (match List.find t.text ~f:(fun word -> word.id = id) with
     | None -> t
     | Some word ->
       let len = String.length word.data in
       if offset < len
       then (
         let c' = word.data.[offset] in
         if Char.equal c c' then { t with cursor = Cursor.make (id, offset + 1) } else t)
       else t)
;;

let place img (x, y) on_ =
  let open Notty.Infix in
  let module I = Notty.I in
  let module A = Notty.A in
  I.hcat [ I.void x 1; I.vcat [ I.void 1 y; img ] ] </> on_
;;

let render t =
  let module I = Notty.I in
  let module A = Notty.A in
  let width, height = Dim.cols_rows t.dim in
  let board = I.char A.empty ' ' width height in
  List.fold
    t.text
    ~init:board
    ~f:(fun board { id; col; row; line_offset; data; state; typed } ->
      let len = String.length data in
      let len_typed = String.length typed in
      let rec loop i board =
        let pos = line_offset + i, row in
        if i < len
        then (
          let attr =
            match state with
            | `New | `Pending -> A.empty
            | `Active -> if i < len_typed then A.fg A.lightgreen else A.empty
            | `Success -> A.fg A.green
            | `Failure -> A.fg A.red
          in
          let board = place (I.string attr (String.of_char data.[i])) pos board in
          loop (i + 1) board)
        else board
      in
      loop 0 board)
;;

let get dim ~state_dir ~corpus =
  let prev_words, next_words, triples, bigram_times, word_times =
    match restore_state ~state_dir with
    | Some state ->
      ( state.prev_words
      , state.next_words
      , state.triples
      , state.bigram_times
      , state.word_times )
    | None ->
      ( Map.empty (module String)
      , Map.empty (module String)
      , Map.empty (module String)
      , Map.empty (module String)
      , Map.empty (module String) )
  in
  create
    ~dim
    ~cursor:(Cursor.make (0, 0))
    ~text:(Corpus.next corpus)
    ~mode:`Main
    ~prev_words
    ~next_words
    ~triples
    ~bigram_times
    ~word_times
    ~corpus
;;
