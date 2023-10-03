open! Import
include Tuple.Make (Int) (Int)
include Tuple.Comparable (Int) (Int)

let ( + ) (x1, y1) (x2, y2) = x1 + x2, y1 + y2
