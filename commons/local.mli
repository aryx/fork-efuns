(*s: commons2/local.mli *)

(*s: signature type Local.vars *)
type vars
(*e: signature type Local.vars *)
(*s: signature function Local.vars *)
(* constructor *)
val vars : unit -> vars
(*e: signature function Local.vars *)

(*s: signature type Local.var *)
type 'a var
(*e: signature type Local.var *)
(*s: signature functions Local.create_xxx *)
val create_int : string -> int var
val create_string : string -> string var
val create_float : string -> float var
(*x: signature functions Local.create_xxx *)
val create_abstr : string -> 'a var
(*e: signature functions Local.create_xxx *)
(* internal *)
(*s: signature function Local.create *)
val create : string -> ('a -> string) -> (string -> 'a) -> 'a var
(*e: signature function Local.create *)

(*s: signature function Local.get *)
val get : vars -> 'a var -> 'a
(*e: signature function Local.get *)
(*s: signature function Local.set *)
val set : vars -> 'a var -> 'a -> unit
(*e: signature function Local.set *)

(*s: signature functions Local *)
val no_input : string -> 'a
val no_print : 'a -> string

val set_input : vars -> 'a var -> string -> unit
val get_print : vars -> 'a var -> string
val list : vars -> string list

val print : vars -> string -> string
val input : vars -> string -> string -> unit
(*e: signature functions Local *)
(*e: commons2/local.mli *)
