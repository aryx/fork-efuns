
(* load/save/kill *)

val save_buffers_and_action :
  Efuns.frame -> ('a * Efuns.buffer) list -> (Efuns.frame -> unit) -> unit
val save_some_buffers : Efuns.action

val exit_efuns : Efuns.action

(* buffers *)

val load_buffer : Efuns.action
val insert_file : Efuns.action

val write_buffer : Efuns.action
val save_buffer : Efuns.action

val window_load_buffer : Efuns.action

val change_buffer : Efuns.action

val window_change_buffer : Efuns.action

(* time *)

val buf_mtime : float Local.var
val update_time : Efuns.buffer -> unit

(* colors/fonts *)

val change_font : Efuns.action

(* variables *)

val set_local_variable : Efuns.action
val set_global_variable : Efuns.action
val describe_variable : Efuns.action

val all_variables : Efuns.frame -> 'a -> string list

(* parameters *)

val set_parameter : Efuns.action
val get_parameter : Efuns.action

(* misc *)

val reload : Efuns.action
val check_file : Efuns.action

val open_display : 'a -> 'b

val goto_line : Efuns.action
val goto_char : Efuns.action

val describe_position : Efuns.action

val mark_at_point : Efuns.action

val umask : int
val file_perm : Unix.file_perm Local.var
val mkdir : Efuns.action

val eval : Efuns.action

(*
val all_vars : (Efuns.frame * string list) option ref

val parameters_hist : string list ref
val variable_hist : string list ref
val value_hist : string list ref
val eval_history : string list ref

*)
