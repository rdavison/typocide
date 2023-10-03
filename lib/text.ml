open! Import

module Word = struct
  type t =
    { id : int
    ; col : int
    ; row : int
    ; line_offset : int
    ; word : string
    ; typed : string
    ; state : [ `New | `Pending | `Active | `Success | `Failure ]
    }
  [@@deriving sexp]
end

type t = Word.t list [@@deriving sexp]
