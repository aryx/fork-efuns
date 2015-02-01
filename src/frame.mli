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

val status_format : (Efuns.status_info * (int * int)) list ref
val status_print : Efuns.status -> string -> Efuns.status_info -> unit
val status_modified : Efuns.frame -> bool -> unit
val status_col : Efuns.frame -> int -> unit
val print_list : string list -> string
val status_major_mode : Efuns.frame  -> unit
val status_line : Efuns.frame -> int -> unit
val status_name : Efuns.frame -> string -> unit
val status_file : Efuns.frame -> string -> unit
val kill : Efuns.frame -> unit
val kill_all : Efuns.window -> unit
val install : Efuns.window -> Efuns.frame -> unit
val resize : Efuns.frame -> unit
val editname : string
val create_without_top :
  Efuns.location ->
  Efuns.window -> string option -> Efuns.buffer -> Efuns.frame
val active : Efuns.frame -> unit
val create : Efuns.window -> string option -> Efuns.buffer -> Efuns.frame
val create_inactive : Efuns.window -> Efuns.buffer -> Efuns.frame
val point_to_cursor : Efuns.buffer -> Text.point -> int
val cursor_to_point : Efuns.frame -> int -> int -> int * int
val update_line : Efuns.top_window -> Efuns.frame -> string -> int -> unit
val set_cursor : Efuns.frame -> unit
val update_table : 'a -> Efuns.frame -> unit
val update : Efuns.top_window -> Efuns.frame -> unit
exception BufferKilled
val unkill : Efuns.window -> Efuns.frame -> unit
val move_point : Efuns.frame -> Text.point -> int -> int -> unit
val current_dir : Efuns.frame -> string
exception FoundFrame of Efuns.frame
val find_buffer_frame : Efuns.location -> Efuns.buffer -> Efuns.frame
val load_file : Efuns.window -> string -> Efuns.frame
val change_buffer : Efuns.window -> string -> unit
val save_buffer : Efuns.frame -> unit
val bindings_help : Efuns.frame -> unit

