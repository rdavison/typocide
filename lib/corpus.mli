open! Import

type t [@@deriving sexp]

val make : string option -> t
val next : t -> string
