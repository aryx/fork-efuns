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

val case_fold : bool ref
type query = NoQuery | Query of Efuns.frame * string
val replace :
  Efuns.to_regexp ->
  Efuns.frame -> query -> string -> string -> unit
val no_query : (bool -> 'a) -> 'a
val query : Efuns.frame -> string -> (bool -> unit) -> unit
val string_history : string list ref
val select_replace : Efuns.frame -> string -> (string -> unit) -> unit
val replace_string : Efuns.frame -> unit
val query_replace_string : Efuns.frame -> unit
val replace_regexp : Efuns.frame -> unit
val query_replace_regexp : Efuns.frame -> unit
val library_regexp : Str.regexp
val library_file : string -> bool
val select_lib_filename : Efuns.frame -> string -> (string -> unit) -> unit
val last_search : string ref
val isearch : Efuns.to_regexp -> Efuns.sens -> Efuns.frame -> unit
val isearch_forward_regexp : Efuns.frame -> unit
val isearch_forward : Efuns.frame -> unit
val isearch_backward : Efuns.frame -> unit
val isearch_backward_regexp : Efuns.frame -> unit

