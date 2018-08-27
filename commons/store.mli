(*s: commons/store.mli *)

(*s: signature type [[Store.t]] *)
type t
(*e: signature type [[Store.t]] *)
(*s: signature function [[Store.new_store]] *)
(* constructor *)
val new_store : unit -> t
(*e: signature function [[Store.new_store]] *)

(*s: signature type [[Store.var]] *)
type 'a var
(*e: signature type [[Store.var]] *)
(*s: signature functions [[Store.create_xxx]] *)
val create_int : string -> int var
val create_string : string -> string var
val create_float : string -> float var
(*x: signature functions [[Store.create_xxx]] *)
val create_abstr : string -> 'a var
(*e: signature functions [[Store.create_xxx]] *)
(* internal *)
(*s: signature function [[Store.create]] *)
val create : string -> ('a -> string) -> (string -> 'a) -> 'a var
(*e: signature function [[Store.create]] *)

(*s: signature function [[Store.get]] *)
val get : t -> 'a var -> 'a
(*e: signature function [[Store.get]] *)
(*s: signature function [[Store.set]] *)
val set : t -> 'a var -> 'a -> unit
(*e: signature function [[Store.set]] *)

(*s: signature functions [[Store]] *)
val no_input : string -> 'a
val no_print : 'a -> string

val set_input : t -> 'a var -> string -> unit
val get_print : t -> 'a var -> string
val list : t -> string list

val print : t -> string -> string
val input : t -> string -> string -> unit
(*e: signature functions [[Store]] *)
(*e: commons/store.mli *)
