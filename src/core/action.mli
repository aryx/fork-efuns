
(* aliases *)
type t = Efuns.action
type name = Efuns.action_name

(* Actions allow to go from a string to an OCaml function
 * (OCaml does not have 'eval', so we need to define "actions").
 * Now I use the [@@interactive] attribute to automate calls to define_action.
 * See ppx/ppx_interactive.ml for more information.
 *)

val define_action : name -> t -> unit
val define_buffer_action : name -> (Efuns.buffer -> unit) -> unit

val get_action : name -> Efuns.generic_action

val execute_action : name -> Efuns.frame -> unit
val execute_buffer_action : name -> Efuns.buffer -> unit


(* should be used only for M-x, and for debugging *)
val actions : (name, Efuns.generic_action) Hashtbl.t
