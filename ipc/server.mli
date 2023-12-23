
type command =
  | LoadFile of string (* filename *) * int (* pos *) * int (* line *) * string

(* assumes Globals.editor has been set *)
val start: unit -> unit

val server_start: Efuns.action

