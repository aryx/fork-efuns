
type 'a t = 'a Store.var

val set_global : 'a t -> 'a -> unit
val set_local : Efuns.buffer -> 'a t -> 'a -> unit

val get_global : 'a t -> 'a
val get_local : Efuns.buffer -> 'a t -> 'a

val get_var : Efuns.buffer -> 'a t -> 'a

val set_minor_var : Efuns.minor_mode -> 'a t -> 'a -> unit
val set_major_var : Efuns.major_mode -> 'a t -> 'a -> unit
