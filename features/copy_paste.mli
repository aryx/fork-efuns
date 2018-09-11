(* cut/copy/paste *)

val kill_string : string -> unit
val kill_text : Text.t -> Text.point -> Text.delta -> unit
val kill_end_of_line : Efuns.action
val kill_eol : Efuns.buffer -> Text.point -> unit
val kill_bol : Efuns.buffer -> Text.point -> unit

val kill_region : Efuns.action
val copy_region : Efuns.action
val insert_killed : Efuns.action
val insert_next_killed : Efuns.action
(*
val kill_size : int ref
val kill_ring : string array
val kill_max : int
val last_kill : (Text.t * Text.position) option ref

val last_insert : (Efuns.frame * Text.position * int * Text.delta) option ref
*)
