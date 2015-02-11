(***********************************************************************)
(*                                                                     *)
(*                             ____________                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

type vars
val vars : unit -> vars

type 'a var
val create : string -> ('a -> string) -> (string -> 'a) -> 'a var
val create_abstr : string -> 'a var
val create_int : string -> int var
val create_string : string -> string var
val create_float : string -> float var

val get : vars -> 'a var -> 'a
val set : vars -> 'a var -> 'a -> unit
val no_input : string -> 'a
val no_print : 'a -> string

val set_input : vars -> 'a var -> string -> unit
val get_print : vars -> 'a var -> string
val list : vars -> string list

val print : vars -> string -> string
val input : vars -> string -> string -> unit