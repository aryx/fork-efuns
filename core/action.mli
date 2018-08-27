
type t = Efuns.action

(* to go from a string to an ocaml function (ocaml does not have eval
 * so we have to assist and define actions) 
 *)

val define_action : Efuns.action_name -> Efuns.action -> unit
val define_buffer_action : Efuns.action_name -> (Efuns.buffer -> unit) -> unit

val get_action : Efuns.action_name -> Efuns.generic_action

val execute_action : Efuns.action_name -> Efuns.frame -> unit
val execute_buffer_action : Efuns.action_name -> Efuns.buffer -> unit


(* internal, should be used only for debugging *)
val actions : (Efuns.action_name, Efuns.generic_action) Hashtbl.t
