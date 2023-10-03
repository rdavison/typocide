open! Import

type word =
  { id : int
  ; col : int
  ; row : int
  ; line_offset : int
  ; word : string
  ; typed : string
  ; state : [ `New | `Pending | `Active | `Success | `Failure ]
  }
[@@deriving sexp]

type text = word list [@@deriving sexp]

type t = private
  { dim : Pos.t
  ; cursor : Pos.t
  ; text : text
  ; problem_words : int String.Map.t
  ; prev_words : int String.Map.t String.Map.t
  ; next_words : int String.Map.t String.Map.t
  ; triples : int String.Map.t String.Map.t
  ; mode : [ `Main | `Practice of string list ]
  }
[@@deriving sexp]

val create
  :  dim:Pos.t
  -> cursor:Pos.t
  -> text:string
  -> mode:[ `Main | `Practice of string list ]
  -> prev_words:int String.Map.t String.Map.t
  -> next_words:int String.Map.t String.Map.t
  -> triples:int String.Map.t String.Map.t
  -> t

val set_dim : t -> Pos.t -> t
val render : t -> Notty.image
val set_cursor : t -> Pos.t -> t
val handle_keypress : t -> char -> t
val process_endgame : t -> t
val process_tab : t -> t
