
val insert_special_char : Efuns.action

val line_comment : string Var.t

val exit_hooks: (unit -> unit) list Var.t
val exit : Efuns.action

(* time *)

val buf_mtime : float Var.t
val update_time : Efuns.buffer -> unit

(* misc *)

val reload : Efuns.action
val check_file : Efuns.action

val goto_line : Efuns.action
val goto_char : Efuns.action

val describe_position : Efuns.action
val cursor_position : Efuns.action
