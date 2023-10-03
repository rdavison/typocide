open! Import

module Keylog : sig
  type t =
    { keycode : string
    ; time : Time_float_unix.t
    }
  [@@deriving sexp]

  val make : ?time:Time_float_unix.t -> string -> t
end

module Word : sig
  type t =
    { id : int
    ; col : int
    ; row : int
    ; line_offset : int
    ; data : string
    ; typed : string
    ; log : Keylog.t list
    ; state : [ `New | `Pending | `Active | `Success | `Failure ]
    }
  [@@deriving sexp]
end

type t = Word.t list [@@deriving sexp]

val col_row_of_cursor : t -> cursor:Cursor.t -> (int * int) option
