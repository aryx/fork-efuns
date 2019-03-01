
(* to create a (typed) hook, do:
 *   let my_hook = Store.create_abstr "my_hook"
 * to add a hook, do (usually in the start_hook of your module):
 *   Hook.add_hook my_hook (fun args -> ...)
 * then to execute the hook, do:
 *   Hook.exec_hooks (Var.get_global_var my_hook) args
 *)

(* this is using Var.set_global, so the hook is global *)
val add_hook : 'a list Var.t -> 'a -> unit

val exec_hooks : ('a -> unit) list -> 'a -> unit

val add_start_hook : (unit -> unit) -> unit
(* should be used only in main.ml; use add_start_hook otherwise *)
val start_hooks : (unit -> unit) list ref

val exec_named_hooks: Efuns.action_name list -> Efuns.frame -> unit
val exec_named_buf_hooks: Efuns.action_name list -> Efuns.buffer -> unit
val exec_named_buf_hooks_with_abort: 
  Efuns.action_name list -> Efuns.buffer -> unit
