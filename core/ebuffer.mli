(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

val create_buf_hook : (Efuns.buffer -> unit) list Local.var
val modes_alist : (string * Efuns.major_mode) list Local.var
val create_syntax_table : unit -> bool array
val default_syntax_table : bool array
val get_name : Efuns.location -> string -> string
val fondamental_mode : Efuns.major_mode
val tab_size : int ref
val create : Efuns.location -> string -> string option ->
  Text.t -> Efuns.map -> Efuns.buffer
val kill : Efuns.location -> Efuns.buffer -> unit
val save : Efuns.buffer -> unit
exception Found of Efuns.buffer
val read : Efuns.location -> string -> Efuns.map -> Efuns.buffer
val default : Efuns.location -> string -> Efuns.buffer
val compute_representation : Efuns.buffer -> int -> Text.line
exception BufferAlreadyOpened
val change_name : Efuns.location -> Efuns.buffer -> string -> unit
val set_mark : Efuns.buffer -> Text.point -> unit
val get_mark : Efuns.buffer -> Text.point -> Text.point
val remove_mark : Efuns.buffer -> unit
val modes_old : (string * Efuns.major_mode) list ref
val regexp_alist : (Str.regexp * Efuns.major_mode) list ref
val set_major_mode : Efuns.buffer -> Efuns.major_mode -> unit
val set_minor_mode : Efuns.buffer -> Efuns.minor_mode -> unit
val del_minor_mode : Efuns.buffer -> Efuns.minor_mode -> unit
val modep : Efuns.buffer -> Efuns.minor_mode -> bool
val suffix_reg : Str.regexp
val set_buffer_mode : Efuns.buffer -> unit
val get_binding : Efuns.buffer -> Efuns.key list -> Efuns.binding
val message : Efuns.buffer -> string -> unit
val catch : 
  (string -> string, unit, string) format ->
  Efuns.buffer -> (unit -> unit) -> unit
val new_major_mode : string -> (Efuns.buffer -> unit) list ->  Efuns.major_mode
val new_minor_mode : string ->  (Efuns.buffer -> unit) list -> Efuns.minor_mode
  