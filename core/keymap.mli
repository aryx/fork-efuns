
(* creation *)

val create: unit -> Efuns.map

(* getters/setters *)

val add_binding: 
  Efuns.map -> Efuns.keys -> Efuns.action -> unit
val add_global_key:
  Efuns.keys -> Efuns.action_name -> Efuns.action -> unit
val add_major_key: 
  Efuns.major_mode -> Efuns.keys -> Efuns.action_name -> Efuns.action -> unit

val add_interactive: Efuns.map -> string -> Efuns.action -> unit
val define_interactive_action: Efuns.action_name -> Efuns.action -> unit

val get_binding: Efuns.map -> Efuns.keys -> Efuns.binding

(* helpers *)

val interactive: 
  Efuns.map -> 
  (Efuns.keys -> Efuns.action_name -> Efuns.action -> unit)

val dummy_action: Efuns.action

(* dumpers *)

val print_key_list: Efuns.keys -> string
val print_key: Efuns.key -> string
val all_bindings: unit -> string

(* common prefixes *)
val c_c: Efuns.key
