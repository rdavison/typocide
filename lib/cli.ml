open! Import
module Term = Notty_async.Term

let cursor_to_col_row cursor ~(text : Text.t) =
  let wordnum, offset = Cursor.id_offset cursor in
  List.find_map text ~f:(fun word ->
    if wordnum = word.id
    then
      Some
        ( word.line_offset
          + Int.max offset (Int.min (String.length word.word) (String.length word.typed))
        , word.row )
    else None)
;;

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
           ~dim:(Dim.make (Term.size term))
           ~cursor:(Cursor.make (0, 0))
           ~text:(Quote.next_text ())
           ~mode:`Main
           ~prev_words:m.prev_words
           ~next_words:m.next_words
           ~triples:m.triples)
       else
         Model.create
           ~dim:(Dim.make (Term.size term))
           ~cursor:(Cursor.make (0, 0))
           ~text:(Quote.next_text ())
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
          | `Tab, [] -> m := Model.process_tab !m
          | _ -> ())
       | `Resize size -> m := Model.set_dim !m (Dim.make size)
       | _ -> ()));
  Clock.every' (sec 0.05) ~stop (fun () ->
    let%bind () = Term.image term (Model.render !m) in
    let%bind () = Term.cursor term (cursor_to_col_row !m.cursor ~text:!m.text) in
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
