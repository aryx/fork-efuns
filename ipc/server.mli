
type command =
  | LoadFile of Common.filename * int (* pos *) * int (* line *) * string

(* assumes Globals.editor has been set if passed None *)
val start: Efuns.frame option -> unit

val server_start: Efuns.action

