(* see Efuns.map, Efuns.keys, Efuns.action types *)
type t = Efuns.map

(* creation *)

val create: unit -> t

(* getters/setters *)

val add_binding: 
  t ->        Efuns.keys ->                      Efuns.action -> unit

val add_global_key:
                      Efuns.keys -> Efuns.action_name -> Efuns.action -> unit
val add_local_key:
 Efuns.buffer ->      Efuns.keys -> Efuns.action_name -> Efuns.action -> unit
val add_major_key: 
  Efuns.major_mode -> Efuns.keys -> Efuns.action_name -> Efuns.action -> unit
val add_minor_key: 
  Efuns.minor_mode -> Efuns.keys -> Efuns.action_name -> Efuns.action -> unit

val define_interactive_action: 
  Efuns.action_name -> Efuns.action -> unit

val add_interactive: 
  t -> Efuns.action_name -> Efuns.action -> unit

val get_binding: t -> Efuns.keys -> Efuns.binding

(* helpers *)

val interactive: 
  t -> 
  (Efuns.keys -> Efuns.action_name -> Efuns.action -> unit)

val dummy_action: Efuns.action

(* dumpers *)

val print_key_list: Efuns.keys -> string
val print_key: Efuns.key -> string
val all_bindings: unit -> string

(* common prefixes *)
val c_x: Efuns.key
val c_c: Efuns.key
val c_h: Efuns.key
val n_5: Efuns.key

(* key <-> string (stored in config/option file) *)
val binding_option :
  (Efuns.key list * Efuns.action_name) Options.option_class
