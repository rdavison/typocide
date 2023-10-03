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
  }
[@@deriving sexp]

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
    { Text.Word.id; col; row; line_offset; data = word; typed = ""; state = `Pending })
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

let create ~dim ~cursor ~text ~mode ~prev_words ~next_words ~triples =
  let text =
    sanitize_text text
    |> make_text ~dim
    |> List.map ~f:(fun word -> if word.id = 0 then { word with state = `New } else word)
  in
  let problem_words = Map.empty (module String) in
  { dim; cursor; text; mode; problem_words; prev_words; next_words; triples }
;;

let make_practice_text t word =
  let practice_len = 10 in
  match Map.find t.prev_words word, Map.find t.next_words word with
  | None, None -> List.init practice_len ~f:(Fn.const word) |> String.concat ~sep:" "
  | Some prevs, Some nexts ->
    let prevs =
      Map.to_alist prevs
      |> List.fold ~init:[] ~f:(fun acc (prev, count) ->
        List.init count ~f:(Fn.const prev) @ acc)
      |> Array.of_list
    in
    let prev_len = Array.length prevs in
    let nexts =
      Map.to_alist nexts
      |> List.fold ~init:[] ~f:(fun acc (next, count) ->
        List.init count ~f:(Fn.const next) @ acc)
      |> Array.of_list
    in
    let next_len = Array.length nexts in
    let prevs =
      List.init practice_len ~f:(fun _ -> prevs.(Random.int prev_len))
      |> List.map ~f:(fun prev -> sprintf "%s %s" prev word)
    in
    let nexts =
      List.init practice_len ~f:(fun _ -> nexts.(Random.int next_len))
      |> List.map ~f:(fun next -> sprintf "%s %s" word next)
    in
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
    ignore prevs;
    ignore nexts;
    let all = triples |> Array.of_list in
    let len = Array.length all in
    List.init practice_len ~f:(fun _ -> all.(Random.int len)) |> String.concat ~sep:" "
  | Some prevs, None ->
    let prevs =
      Map.to_alist prevs
      |> List.fold ~init:[] ~f:(fun acc (prev, count) ->
        List.init count ~f:(Fn.const prev) @ acc)
      |> Array.of_list
    in
    let prev_len = Array.length prevs in
    List.init practice_len ~f:(fun _ -> prevs.(Random.int prev_len))
    |> List.concat_map ~f:(fun prev -> [ prev; word ])
    |> String.concat ~sep:" "
  | None, Some nexts ->
    let nexts =
      Map.to_alist nexts
      |> List.fold ~init:[] ~f:(fun acc (next, count) ->
        List.init count ~f:(Fn.const next) @ acc)
      |> Array.of_list
    in
    let next_len = Array.length nexts in
    List.init practice_len ~f:(fun _ -> nexts.(Random.int next_len))
    |> List.concat_map ~f:(fun next -> [ word; next ])
    |> String.concat ~sep:" "
;;

let update_problems t =
  let arr = Array.of_list t.text in
  let len = Array.length arr in
  let problem_words = ref t.problem_words in
  let prev_words = ref t.prev_words in
  let next_words = ref t.next_words in
  let triples = ref t.triples in
  for i = 0 to len - 1 do
    let prev = i - 1 in
    let next = i + 1 in
    let focus = arr.(i) in
    if (not (String.is_empty focus.typed)) && not (String.equal focus.data focus.typed)
    then (
      problem_words
        := Map.update !problem_words focus.data ~f:(function
             | None -> 1
             | Some n -> n + 1);
      prev_words
        := if prev < 0
           then !prev_words
           else (
             let prev_word = arr.(prev).data in
             Map.update !prev_words focus.data ~f:(function
               | None -> Map.singleton (module String) prev_word 1
               | Some map ->
                 Map.update map prev_word ~f:(function
                   | None -> 1
                   | Some n -> n + 1)));
      next_words
        := if next >= len
           then !next_words
           else (
             let next_word = arr.(next).data in
             Map.update !next_words focus.data ~f:(function
               | None -> Map.singleton (module String) next_word 1
               | Some map ->
                 Map.update map next_word ~f:(function
                   | None -> 1
                   | Some n -> n + 1)));
      triples
        := if prev < 0 || next >= len
           then !triples
           else (
             let triple =
               let prev_word = arr.(prev).data in
               let next_word = arr.(next).data in
               sprintf "%s %s %s" prev_word focus.data next_word
             in
             Map.update !triples focus.data ~f:(function
               | None -> Map.singleton (module String) triple 1
               | Some map ->
                 Map.update map triple ~f:(function
                   | None -> 1
                   | Some n -> n + 1))))
  done;
  { t with
    problem_words = !problem_words
  ; prev_words = !prev_words
  ; next_words = !next_words
  ; triples = !triples
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
    match t.mode with
    | `Main | `Practice [] ->
      if current_word.id = 0
      then (
        match current_word.state with
        | `New -> `Main, Corpus.next (), t
        | _ ->
          let t = restart_game t in
          t.mode, List.map t.text ~f:(fun word -> word.data) |> String.concat ~sep:" ", t)
      else `Main, List.map t.text ~f:(fun word -> word.data) |> String.concat ~sep:" ", t
    | `Practice (next_text :: rest) ->
      if current_word.id = 0
      then (
        match current_word.state with
        | `New -> `Practice rest, make_practice_text t next_text, t
        | _ ->
          let t = restart_game t in
          t.mode, List.map t.text ~f:(fun word -> word.data) |> String.concat ~sep:" ", t)
      else t.mode, List.map t.text ~f:(fun word -> word.data) |> String.concat ~sep:" ", t
  in
  create
    ~dim:t.dim
    ~cursor:(Cursor.make (0, 0))
    ~text
    ~mode
    ~prev_words:t.prev_words
    ~next_words:t.next_words
    ~triples:t.triples
;;

let process_endgame t =
  let mode, text, t =
    match t.mode with
    | `Main ->
      let t = update_problems t in
      let problem_words = Map.to_alist t.problem_words |> List.map ~f:fst in
      (match problem_words with
       | fst :: rest -> `Practice rest, make_practice_text t fst, t
       | [] -> `Main, Corpus.next (), t)
    | `Practice [] ->
      if should_repeat t
      then
        ( `Practice []
        , List.map t.text ~f:(fun word -> word.data) |> String.concat ~sep:" "
        , t )
      else `Main, Corpus.next (), t
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
          { word with state })
        else if word.id = id'
        then { word with state = `Active }
        else word)
    in
    let t = { t with text } in
    if id' >= List.length t.text then process_endgame t else t
  | _ ->
    let text =
      List.map t.text ~f:(fun word ->
        if word.id = id
        then { word with typed = word.typed ^ String.of_char c; state = `Active }
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

let restore_state () =
  if Sys_unix.file_exists_exn "save_state.sexp"
  then Some (Core.In_channel.read_all "save_state.sexp" |> Sexp.of_string |> t_of_sexp)
  else None
;;

let get dim =
  let prev_words, next_words, triples =
    match restore_state () with
    | Some state -> state.prev_words, state.next_words, state.triples
    | None ->
      Map.empty (module String), Map.empty (module String), Map.empty (module String)
  in
  create
    ~dim
    ~cursor:(Cursor.make (0, 0))
    ~text:(Corpus.next ())
    ~mode:`Main
    ~prev_words
    ~next_words
    ~triples
;;

let save_state t =
  Stdio.Out_channel.write_all "save_state.sexp" ~data:(sexp_of_t t |> Sexp.to_string)
;;
