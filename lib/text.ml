open! Import

module Word = struct
  type t =
    { id : int
    ; col : int
    ; row : int
    ; line_offset : int
    ; data : string
    ; typed : string
    ; state : [ `New | `Pending | `Active | `Success | `Failure ]
    }
  [@@deriving sexp]
end

type t = Word.t list [@@deriving sexp]

let col_row_of_cursor (t : t) ~cursor =
  let id, offset = Cursor.id_offset cursor in
  List.find_map t ~f:(fun word ->
    if id = word.id
    then
      Some
        ( word.line_offset
          + Int.max offset (Int.min (String.length word.data) (String.length word.typed))
        , word.row )
    else None)
;;
