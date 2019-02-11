(***********************************************************************)
(*                                                                     *)
(*                    ______________                                   *)
(*                                                                     *)
(*      Fabrice Le Fessant, projet SOR/PARA INRIA Rocquencourt         *)
(*                                                                     *)
(*                 mail: fabrice.le_fessant@inria.fr                   *)
(*                 web:  http://www-sor.inria.fr/~lefessan             *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*                                                                     *)
(***********************************************************************)

val iterator : int ref -> unit
val poll : unit -> bool

val add_reader : Unix.file_descr -> (unit -> unit) -> unit 
val remove_reader : Unix.file_descr -> unit
val add_timer : float -> (unit -> unit) -> unit
val fork : unit -> int
