
val insert_special_char : Efuns.action

val line_comment : string Store.var

val window_load_buffer : Efuns.action
val window_change_buffer : Efuns.action

val exit : Efuns.action

(* time *)

val buf_mtime : float Store.var
val update_time : Efuns.buffer -> unit

(* misc *)

val reload : Efuns.action
val check_file : Efuns.action

val goto_line : Efuns.action
val goto_char : Efuns.action

val describe_position : Efuns.action
val cursor_position : Efuns.action
