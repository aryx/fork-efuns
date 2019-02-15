
type t = Efuns.action

(* Actions allow to go from a string to an OCaml function
 * (OCaml does not have 'eval', so we need to define "actions").
 * Now I use the [@@interactive] attribute to automate calls to define_action.
 * See ppx/ppx_interactive.ml for more information.
 *)

val define_action : Efuns.action_name -> t -> unit
val define_buffer_action : Efuns.action_name -> (Efuns.buffer -> unit) -> unit

val get_action : Efuns.action_name -> Efuns.generic_action

val execute_action : Efuns.action_name -> Efuns.frame -> unit
val execute_buffer_action : Efuns.action_name -> Efuns.buffer -> unit


(* should be used only for M-x, and for debugging *)
val actions : (Efuns.action_name, Efuns.generic_action) Hashtbl.t
