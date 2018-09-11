
val insert_special_char : Efuns.action
val fill_paragraph : Efuns.action
(*
val single_char : string
val simplify : Text.t -> Text.point -> Text.point -> unit
*)

val line_comment : string Store.var

(* buffers *)

val load_buffer : Efuns.action
val insert_file : Efuns.action

val write_buffer : Efuns.action
val save_buffer : Efuns.action

val window_load_buffer : Efuns.action
val window_change_buffer : Efuns.action

(* load/save/kill *)

val save_buffers_and_action :
  Efuns.frame -> ('a * Efuns.buffer) list -> (Efuns.frame -> unit) -> unit
val save_some_buffers : Efuns.action

val exit_efuns : Efuns.action

(* time *)

val buf_mtime : float Store.var
val update_time : Efuns.buffer -> unit

(* parameters *)

val set_parameter : Efuns.action
val get_parameter : Efuns.action

(* val parameters_hist : string list ref *)

(* misc *)

val reload : Efuns.action
val check_file : Efuns.action

val goto_line : Efuns.action
val goto_char : Efuns.action

val describe_position : Efuns.action
val cursor_position : Efuns.action
