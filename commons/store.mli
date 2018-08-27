(*s: commons/store.mli *)

(*s: signature type [[Store.vars]] *)
type vars
(*e: signature type [[Store.vars]] *)
(*s: signature function [[Store.vars]] *)
(* constructor *)
val vars : unit -> vars
(*e: signature function [[Store.vars]] *)

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
val get : vars -> 'a var -> 'a
(*e: signature function [[Store.get]] *)
(*s: signature function [[Store.set]] *)
val set : vars -> 'a var -> 'a -> unit
(*e: signature function [[Store.set]] *)

(*s: signature functions [[Store]] *)
val no_input : string -> 'a
val no_print : 'a -> string

val set_input : vars -> 'a var -> string -> unit
val get_print : vars -> 'a var -> string
val list : vars -> string list

val print : vars -> string -> string
val input : vars -> string -> string -> unit
(*e: signature functions [[Store]] *)
(*e: commons/store.mli *)
