open! Import

type t [@@deriving sexp]

val make : int * int -> t
val id_offset : t -> int * int
