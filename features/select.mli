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

val select_yes_or_no : Efuns.frame -> string -> (bool -> unit) -> Efuns.frame
val display_completions : Efuns.frame -> string list -> unit
val remove_completions : Efuns.frame -> unit
val set_history : Efuns.map -> string ref -> string list ref -> unit
val incremental_mini_buffer :
  Efuns.frame ->
  Efuns.map ->
  string ->
  string ->
  (Efuns.frame -> string -> unit) ->
  (Efuns.frame -> string -> unit) -> Efuns.frame
val select :
  Efuns.frame ->
  string ->
  string list ref ->
  string ->
  (string -> string list) -> (string -> string) -> (string -> unit) -> unit
val file_hist : string list ref
val dont_complete : string list Options.option_record
val dont_complete_regexps : (string list * Str.regexp) ref
val dont_complete_regexp : unit -> Str.regexp
val avoid_completion : string -> bool
val is_userdir : string -> bool
val complete_filename :
  Efuns.frame -> (string -> bool) -> string -> string list
val select_file :
  Efuns.frame ->
  string -> string list ref -> string -> (string -> unit) -> unit
val select_filename : Efuns.frame -> string -> (string -> unit) -> unit
val next_default : string ref
val set_previous_frame : Efuns.frame -> unit
val get_previous_frame : unit -> string
val buf_hist : string list ref
val select_buffer : Efuns.frame -> string -> string -> (string -> unit) -> unit
val select_string :
  Efuns.frame ->
  string -> string list ref -> string -> (string -> unit) -> unit
val simple_select : Efuns.frame -> string -> (string -> unit) -> unit
val prev_buffers : string list ref
