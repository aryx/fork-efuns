
val save_buffers_and_action :
  Efuns.frame -> ('a * Efuns.buffer) list -> (Efuns.frame -> unit) -> unit

val buf_mtime : float Local.var
val update_time : Efuns.buffer -> unit

val reload : Efuns.frame -> unit

val check_file : Efuns.frame -> unit

val exit_efuns : Efuns.frame -> unit

val save_some_buffers : Efuns.frame -> unit

val load_buffer : Efuns.frame -> unit
val insert_file : Efuns.frame -> unit

val write_buffer : Efuns.frame -> unit
val save_buffer : Efuns.frame -> unit

val window_load_buffer : Efuns.frame -> unit

val change_buffer : Efuns.frame -> unit

val window_change_buffer : Efuns.frame -> unit

val change_font : Efuns.frame -> unit
val color : Efuns.buffer -> Str.regexp -> bool -> Text.attribute -> unit

val open_display : 'a -> 'b

val goto_line : Efuns.frame -> unit
val goto_char : Efuns.frame -> unit

val get_pos : Efuns.frame -> unit

val mark_at_point : Efuns.frame -> unit

val umask : int
val file_perm : Unix.file_perm Local.var
val mkdir : Efuns.frame -> unit

val eval_history : string list ref
val eval : Efuns.frame -> unit

val variable_hist : string list ref
val value_hist : string list ref

val all_vars : (Efuns.frame * string list) option ref
val all_variables : Efuns.frame -> 'a -> string list

val set_local_variable : Efuns.frame -> unit
val set_global_variable : Efuns.frame -> unit
val get_variable : Efuns.frame -> unit

val parameters_hist : string list ref
val set_parameter : Efuns.frame -> unit
val get_parameter : Efuns.frame -> unit
