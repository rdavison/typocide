open! Import

type t = private
  { dim : Dim.t
  ; cursor : Cursor.t
  ; text : Text.t
  ; problem_words : int String.Map.t
  ; prev_words : int String.Map.t String.Map.t
  ; next_words : int String.Map.t String.Map.t
  ; triples : int String.Map.t String.Map.t
  ; mode : [ `Main | `Practice of string list ]
  ; bigram_times : Time_float.Span.t String.Map.t
  ; word_times : Time_float.Span.t String.Map.t
  ; corpus : Corpus.t
  }
[@@deriving sexp]

val create
  :  dim:Dim.t
  -> cursor:Cursor.t
  -> text:string
  -> mode:[ `Main | `Practice of string list ]
  -> prev_words:int String.Map.t String.Map.t
  -> next_words:int String.Map.t String.Map.t
  -> triples:int String.Map.t String.Map.t
  -> bigram_times:Time_float.Span.t String.Map.t
  -> word_times:Time_float.Span.t String.Map.t
  -> corpus:Corpus.t
  -> t

val set_dim : t -> Dim.t -> t
val render : t -> Notty.image
val set_cursor : t -> Cursor.t -> t
val handle_keypress : t -> char -> t
val process_endgame : t -> t
val process_tab : t -> t
val get : Dim.t -> state_dir:string option -> corpus:Corpus.t -> t
val save_state : t -> state_dir:string option -> unit
