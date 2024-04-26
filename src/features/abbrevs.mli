
val abbrev_table : (string, string) Hashtbl.t Var.t

val char_expand_abbrev: Efuns.action
val expand_sabbrev : Efuns.action
val dabbrev_expand : Efuns.action

(*
val escaped : string -> string
val dabbrev_buf :
  (string * Efuns.frame * Text.position * Text.delta * Efuns.buffer *
   Text.position * string list)
  option ref
*)
