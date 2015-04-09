
val buffer_list : Efuns.frame -> string list

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
