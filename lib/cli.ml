open! Import
module Term = Notty_async.Term

let next_text =
  let texts =
    Stdio.In_channel.read_lines "corpus.txt"
    |> List.filter_map ~f:(fun line ->
      match String.strip line with
      | "" -> None
      | line -> Some line)
    |> Array.of_list
  in
  let len = Array.length texts in
  fun () -> texts.(Random.int len)
;;

module Pos = struct
  include Tuple.Make (Int) (Int)
  include Tuple.Comparable (Int) (Int)

  let ( + ) (x1, y1) (x2, y2) = x1 + x2, y1 + y2
end

module Model : sig
  type word =
    { id : int
    ; col : int
    ; row : int
    ; line_offset : int
    ; word : string
    ; typed : string
    ; state : [ `New | `Active | `Success | `Failure ]
    }
  [@@deriving sexp]

  type text = word list [@@deriving sexp]

  type t =
    { dim : Pos.t
    ; cursor : Pos.t
    ; text : text
    ; problem_words : int String.Map.t
    ; prev_words : int String.Map.t String.Map.t
    ; next_words : int String.Map.t String.Map.t
    ; triples : int String.Map.t String.Map.t
    ; mode : [ `Main | `Practice of string list ]
    }
  [@@deriving sexp]

  val create
    :  dim:Pos.t
    -> cursor:Pos.t
    -> text:string
    -> mode:[ `Main | `Practice of string list ]
    -> prev_words:int String.Map.t String.Map.t
    -> next_words:int String.Map.t String.Map.t
    -> triples:int String.Map.t String.Map.t
    -> t

  val set_dim : t -> Pos.t -> t
  val render : t -> Notty.image
  val set_cursor : t -> Pos.t -> t
  val handle_keypress : t -> char -> t
end = struct
  type word =
    { id : int
    ; col : int
    ; row : int
    ; line_offset : int
    ; word : string
    ; typed : string
    ; state : [ `New | `Active | `Success | `Failure ]
    }
  [@@deriving sexp]

  type text = word list [@@deriving sexp]

  type t =
    { dim : Pos.t
    ; cursor : Pos.t
    ; text : text
    ; problem_words : int String.Map.t
    ; prev_words : int String.Map.t String.Map.t
    ; next_words : int String.Map.t String.Map.t
    ; triples : int String.Map.t String.Map.t
    ; mode : [ `Main | `Practice of string list ]
    }
  [@@deriving sexp]

  open Notty
  open Notty.Infix

  let place img (x, y) on_ = I.hcat [ I.void x 1; I.vcat [ I.void 1 y; img ] ] </> on_
  let set_cursor t cursor = { t with cursor }

  let make_text text ~(dim : Pos.t) : text =
    let cols, rows = dim in
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
      { id; col; row; line_offset; word; typed = ""; state = `New })
  ;;

  let%expect_test "test breaking" =
    let dim = 72, 10 in
    let s =
      make_text (next_text ()) ~dim
      |> List.map ~f:(fun info -> info.word)
      |> String.concat ~sep:" "
    in
    print_endline s;
    [%expect
      {|
    I think that if I ever have kids, and they are upset, I won't tell them that people are starving in China or anything like that because it wouldn't change the fact that they were upset. And even if somebody else has it much worse, that doesn't really change the fact that you have what you have. |}];
    return ()
  ;;

  let remake_text (text : text) ~dim =
    let cols, rows = dim in
    let acc, line, _count =
      text
      |> List.fold ~init:([], [], 0) ~f:(fun (acc, line, cumul) word ->
        let len = String.length word.word + 1 in
        if cumul + len < cols
        then acc, word :: line, cumul + len
        else List.rev line :: acc, [ word ], len)
    in
    let raw = List.rev (List.rev line :: acc) in
    List.mapi raw ~f:(fun row line ->
      let seen = ref 0 in
      List.mapi line ~f:(fun col word ->
        let line_offset = !seen in
        seen := line_offset + String.length word.word + 1;
        { word with col; row; line_offset }))
    |> List.concat
  ;;

  let set_dim t dim = { t with dim; text = remake_text t.text ~dim }

  let sanitize_text text =
    text
    |> String.split ~on:' '
    |> List.filter_map ~f:(fun s ->
      match String.strip s with
      | "" -> None
      | other -> Some other)
    |> String.concat ~sep:" "
  ;;

  let create ~dim ~cursor ~text ~mode ~prev_words ~next_words ~triples =
    let text =
      sanitize_text text
      |> make_text ~dim
      |> List.map ~f:(fun info ->
        if info.id = 0 then { info with state = `Active } else info)
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
      let all = triples @ prevs @ nexts |> Array.of_list in
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
      if not (String.equal focus.word focus.typed)
      then (
        problem_words
          := Map.update !problem_words focus.word ~f:(function
               | None -> 1
               | Some n -> n + 1);
        prev_words
          := if prev < 0
             then !prev_words
             else (
               let prev_word = arr.(prev).word in
               Map.update !prev_words focus.word ~f:(function
                 | None -> Map.singleton (module String) prev_word 1
                 | Some map ->
                   Map.update map prev_word ~f:(function
                     | None -> 1
                     | Some n -> n + 1)));
        next_words
          := if next >= len
             then !next_words
             else (
               let next_word = arr.(next).word in
               Map.update !next_words focus.word ~f:(function
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
                 let prev_word = arr.(prev).word in
                 let next_word = arr.(next).word in
                 sprintf "%s %s %s" prev_word focus.word next_word
               in
               Map.update !triples focus.word ~f:(function
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

  let process_endgame t =
    let mode, text, t =
      match t.mode with
      | `Main ->
        let t = update_problems t in
        let problem_words = Map.to_alist t.problem_words |> List.map ~f:fst in
        (match problem_words with
         | fst :: rest -> `Practice rest, make_practice_text t fst, t
         | [] -> `Main, next_text (), t)
      | `Practice [] -> `Main, next_text (), t
      | `Practice (word :: rest) -> `Practice rest, make_practice_text t word, t
    in
    create
      ~dim:t.dim
      ~cursor:(0, 0)
      ~text
      ~mode
      ~prev_words:t.prev_words
      ~next_words:t.next_words
      ~triples:t.triples
  ;;

  let handle_keypress t c =
    let wordnum, offset = t.cursor in
    match c with
    | ' ' ->
      let t = { t with cursor = wordnum + 1, 0 } in
      let wordnum', offset' = t.cursor in
      let text =
        List.map t.text ~f:(fun info ->
          if info.id = wordnum
          then (
            let state =
              if String.equal info.word info.typed then `Success else `Failure
            in
            { info with state })
          else if info.id = wordnum'
          then { info with state = `Active }
          else info)
      in
      let t = { t with text } in
      if wordnum' >= List.length t.text then process_endgame t else t
    | _ ->
      let text =
        List.map t.text ~f:(fun info ->
          if info.id = wordnum
          then { info with typed = info.typed ^ String.of_char c }
          else info)
      in
      let t = { t with text } in
      (match List.find t.text ~f:(fun info -> info.id = wordnum) with
       | None -> t
       | Some info ->
         let len = String.length info.word in
         if offset < len
         then (
           let c' = info.word.[offset] in
           if Char.equal c c' then { t with cursor = wordnum, offset + 1 } else t)
         else t)
  ;;

  let render t =
    let width, height = t.dim in
    let board = I.char A.empty ' ' width height in
    List.fold
      t.text
      ~init:board
      ~f:(fun board { id; col; row; line_offset; word; state; typed } ->
        let len = String.length word in
        let len_typed = String.length typed in
        let rec loop i board =
          let pos = line_offset + i, row in
          if i < len
          then (
            let attr =
              match state with
              | `New -> A.empty
              | `Active -> if i < len_typed then A.fg A.green else A.empty
              | `Success -> A.fg A.lightgreen
              | `Failure -> A.fg A.red
            in
            let board = place (I.string attr (String.of_char word.[i])) pos board in
            loop (i + 1) board)
          else board
        in
        loop 0 board)
  ;;
end

let run () =
  let%bind term = Term.create () in
  let events = Term.events term in
  let stop = Pipe.closed events in
  let m =
    ref
      (if Sys_unix.file_exists_exn "save_state.sexp"
       then (
         let m =
           Core.In_channel.read_all "save_state.sexp" |> Sexp.of_string |> Model.t_of_sexp
         in
         Model.create
           ~dim:(Term.size term)
           ~cursor:(0, 0)
           ~text:(next_text ())
           ~mode:`Main
           ~prev_words:m.prev_words
           ~next_words:m.next_words
           ~triples:m.triples)
       else
         Model.create
           ~dim:(Term.size term)
           ~cursor:(0, 0)
           ~text:(next_text ())
           ~mode:`Main
           ~prev_words:(Map.empty (module String))
           ~next_words:(Map.empty (module String))
           ~triples:(Map.empty (module String)))
  in
  don't_wait_for
    (Pipe.iter_without_pushback events ~f:(fun ev ->
       match ev with
       | `Key key ->
         (match key with
          | `ASCII 'C', [ `Ctrl ] ->
            Stdio.Out_channel.write_all
              "save_state.sexp"
              ~data:(Model.sexp_of_t !m |> Sexp.to_string);
            Pipe.close_read events
          | `ASCII c, [] -> m := Model.handle_keypress !m c
          | _ -> ())
       | `Resize size -> m := Model.set_dim !m size
       | _ -> ()));
  Clock.every' (sec 0.05) ~stop (fun () ->
    let%bind () = Term.image term (Model.render !m) in
    return ());
  stop
;;

let command =
  Command.async
    ~summary:"Test of Notty_async"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () -> run ())
    ~behave_nicely_in_pipeline:false
;;
