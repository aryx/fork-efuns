
type 'a t = 'a Store.var

val set_global : 'a Store.var -> 'a -> unit
val set_local : Efuns.buffer -> 'a Store.var -> 'a -> unit

val get_global : 'a Store.var -> 'a
val get_local : Efuns.buffer -> 'a Store.var -> 'a

val get_var : Efuns.buffer -> 'a Store.var -> 'a

val set_minor_var : Efuns.minor_mode -> 'a Store.var -> 'a -> unit
val set_major_var : Efuns.major_mode -> 'a Store.var -> 'a -> unit
