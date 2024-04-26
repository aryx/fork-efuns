
(* interactive with default *)
val change_buffer : Efuns.action
(* go to default *)
val switch_to_other_buffer: Efuns.action

val left_buffer  : Efuns.action
val right_buffer : Efuns.action

(* pinning *)
val down_buffer  : Efuns.action
val up_buffer    : Efuns.action

val kill_buffer : Efuns.action

val buf_hist : string list ref
val select_buffer : 
  Efuns.frame -> string -> string -> (string -> unit) -> unit

(* iterator *)
val next_buffer : Efuns.buffer -> Efuns.buffer

(*val prev_buffers : string list ref*)
val set_previous_frame : Efuns.frame -> unit
val get_previous_frame : unit -> string

(* buffers *)

val load_buffer : Efuns.action
val insert_file : Efuns.action

val write_buffer : Efuns.action
val save_buffer : Efuns.action

(* load/save/kill *)

val save_buffers_and_action :
  Efuns.frame -> ('a * Efuns.buffer) list -> (Efuns.frame -> unit) -> unit
val save_some_buffers : Efuns.action

(* used by buffer_menu.ml *)
val buffer_list : unit -> string list
