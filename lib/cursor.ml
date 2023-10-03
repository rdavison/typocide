open! Import
include Tuple.Make (Int) (Int)
include Tuple.Comparable (Int) (Int)

let make t = t
let id_offset t = t
