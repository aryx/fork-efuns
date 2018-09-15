
val add_hook : 'a list Var.t -> 'a -> unit
val exec_hooks : ('a -> unit) list -> 'a -> unit

val add_start_hook : (unit -> unit) -> unit
(* should be used only in main.ml; use add_start_hook otherwise *)
val start_hooks : (unit -> unit) list ref

val exec_named_hooks: Efuns.action_name list -> Efuns.frame -> unit
val exec_named_buf_hooks: Efuns.action_name list -> Efuns.buffer -> unit
val exec_named_buf_hooks_with_abort: 
  Efuns.action_name list -> Efuns.buffer -> unit
