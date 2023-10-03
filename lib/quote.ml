open! Import

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
