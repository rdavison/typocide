open! Import
module Term = Notty_async.Term

let main ~frames_per_second ~state_dir () =
  let%bind term = Term.create () in
  let events = Term.events term in
  let stop = Pipe.closed events in
  let m = ref (Model.get (Dim.make (Term.size term))) in
  don't_wait_for
    (Pipe.iter_without_pushback events ~f:(function
      | `Key (`ASCII 'C', [ `Ctrl ]) ->
        Model.save_state !m ~state_dir;
        Pipe.close_read events
      | `Key (`ASCII c, []) -> m := Model.handle_keypress !m c
      | `Key (`Tab, []) -> m := Model.process_tab !m
      | `Key _ -> ()
      | `Resize size -> m := Model.set_dim !m (Dim.make size)
      | _ -> ()));
  Clock.every'
    (sec (1.0 /. Float.of_int frames_per_second))
    ~stop
    (fun () ->
      let%bind () = Term.image term (Model.render !m) in
      let%bind () = Term.cursor term (Text.col_row_of_cursor !m.text ~cursor:!m.cursor) in
      return ());
  stop
;;

let command =
  Command.async
    ~summary:"Typing tutor inspired by Leveltype."
    ~behave_nicely_in_pipeline:false
    (let open Command.Let_syntax in
     let%map_open frames_per_second = return 60
     and state_dir =
       flag
         "--state-dir"
         (optional Filename_unix.arg_type)
         ~doc:
           (String.concat
              ~sep:" "
              [ "PATH Path to state directory."
              ; (match Directories.state_dir with
                 | None -> "If not specified, state will not be saved between runs."
                 | Some dir -> sprintf "Default: %s" dir)
              ])
       |> map ~f:(function
         | Some dir -> Some dir
         | None -> Directories.state_dir)
     in
     main ~frames_per_second ~state_dir)
;;
