
val abbrev_table : (string, string) Hashtbl.t Store.var

val expand_sabbrev : Efuns.frame -> unit

val dabbrev_expand : Efuns.frame -> unit

(*
val escaped : string -> string
val dabbrev_buf :
  (string * Efuns.frame * Text.position * Text.delta * Efuns.buffer *
   Text.position * string list)
  option ref
*)
