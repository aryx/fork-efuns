
val create: unit -> Efuns.map

val add_binding: Efuns.map -> Efuns.key list -> Efuns.action -> unit
val get_binding: Efuns.map -> Efuns.key list -> Efuns.binding

val add_interactive: Efuns.map -> string -> Efuns.action -> unit
val interactive: 
  Efuns.map -> 
  (Efuns.prefix -> string -> Efuns.action -> unit)

val add_global_key:
  Efuns.prefix -> string -> Efuns.action -> unit
val add_major_key: 
  Efuns.major_mode -> Efuns.prefix -> string -> Efuns.action -> unit


val dummy_action: Efuns.action

val print_key_list: Efuns.key list -> string
val print_key: Efuns.key -> string
val all_bindings: unit -> string

val c_c: Efuns.key
