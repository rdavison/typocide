open! Import
module Term = Notty_async.Term

let run () =
  let%bind term = Term.create () in
  let events = Term.events term in
  let stop = Pipe.closed events in
  let m = ref (Model.get (Dim.make (Term.size term))) in
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
    let%bind () = Term.cursor term (Text.col_row_of_cursor !m.text ~cursor:!m.cursor) in
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
