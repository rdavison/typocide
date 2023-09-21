open! Import
module Term = Notty_async.Term

let text =
  {|I think that if I ever have kids, and they are upset, I won't tell them that people are starving in China or anything like that because it wouldn't change the fact that they were upset. And even if somebody else has it much worse, that doesn't really change the fact that you have what you have.|}
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

  type text = word list

  type t = private
    { dim : Pos.t
    ; cursor : Pos.t
    ; text : text
    }

  val create : dim:Pos.t -> cursor:Pos.t -> t
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
  [@@deriving sexp_of]

  type text = word list [@@deriving sexp_of]

  type t =
    { dim : Pos.t
    ; cursor : Pos.t
    ; text : text
    }
  [@@deriving sexp_of]

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
      make_text text ~dim |> List.map ~f:(fun info -> info.word) |> String.concat ~sep:" "
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

  let create ~dim ~cursor =
    let text =
      sanitize_text text
      |> make_text ~dim
      |> List.map ~f:(fun info ->
        if info.id = 0 then { info with state = `Active } else info)
    in
    { dim; cursor; text }
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
      { t with text }
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
  let m = ref (Model.create ~dim:(Term.size term) ~cursor:(0, 0)) in
  don't_wait_for
    (Pipe.iter_without_pushback events ~f:(fun ev ->
       match ev with
       | `Key key ->
         (match key with
          | `ASCII 'C', [ `Ctrl ] -> Pipe.close_read events
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
