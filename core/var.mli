
type 'a t = 'a Local.var

val set_global : 'a Local.var -> 'a -> unit
val set_local : Efuns.buffer -> 'a Local.var -> 'a -> unit

val get_global : 'a Local.var -> 'a
val get_local : Efuns.buffer -> 'a Local.var -> 'a

val get_var : Efuns.buffer -> 'a Local.var -> 'a

val set_minor_var : Efuns.minor_mode -> 'a Local.var -> 'a -> unit
val set_major_var : Efuns.major_mode -> 'a Local.var -> 'a -> unit
