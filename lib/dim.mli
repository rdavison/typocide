open! Import

type t [@@deriving sexp]

val make : int * int -> t
val cols_rows : t -> int * int
