
(* M-x *)
val call_interactive : Efuns.action
(* val meta_hist : string list ref *)
(* val value_hist : string list ref *)

(*
val compute_interactives :
  unit -> (string * (Efuns.action * Efuns.keys option)) list
val exec_interactive :
  (string * (Efuns.action * Efuns.keys option)) list ->
  Efuns.frame -> string -> unit
*)

(* help for user *)
val create_bindings_help_buffer : unit -> Efuns.buffer

(* variables *)

val describe_variable : Efuns.action
(* val variable_hist : string list ref *)

val set_local_variable : Efuns.action
val set_global_variable : Efuns.action

val all_variables : Efuns.frame -> 'a -> string list

(* val all_vars : (Efuns.frame * string list) option ref *)

(* parameters *)

val set_parameter : Efuns.action
val get_parameter : Efuns.action

(* val parameters_hist : string list ref *)
