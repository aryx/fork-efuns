
val add_hook : 'a list Local.var -> 'a -> unit
val exec_hooks : ('a -> unit) list -> 'a -> unit

val add_start_hook : (unit -> unit) -> unit
val start_hooks : (unit -> unit) list ref

