open! Import

type t =
  { texts : string array
  ; len : int
  }
[@@deriving sexp]

let make path =
  let lines =
    match path with
    | None -> String.split_lines [%blob "typeracer.txt"]
    | Some path -> Stdio.In_channel.read_lines path
  in
  let texts =
    lines
    |> List.filter_map ~f:(fun line ->
      match String.strip line with
      | "" -> None
      | line -> Some line)
    |> Array.of_list
  in
  let len = Array.length texts in
  { texts; len }
;;

let next { texts; len } = texts.(Random.int len)
