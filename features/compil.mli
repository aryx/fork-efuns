
val compile : Efuns.action
(* val make_hist : string list ref *)

val grep : Efuns.action
(* val grep_hist : string list ref *)
(* val grep_command : string Options.option_record *)

val next_error : Efuns.action

type error = {
  err_filename : string;
  err_line : int;
  err_begin : int;
  err_end : int;
  err_msg : int;
}
type find_error_fun = Text.t -> Text.point -> error

val find_error_gen : Str.regexp -> find_error_fun

(* To setup as major mode variable in the different PL modes.
 * Default to c_error_regexp otherwise.
 *)
val find_error_location_regexp : Str.regexp Var.t

val c_error_regexp : (string * Str.regexp) Options.t

(* to setup if 'find_error_gen find_error_location_regexp' is not enough *)
val find_error : find_error_fun Var.t

(* to colorize the compilation buffer *)
val find_error_error_regexp: Str.regexp Var.t


val compile_find_makefile : bool Options.t

(* internals:
val compilation_frame : (Efuns.frame * Text.point * string) option ref
val default_error : (Text.t -> Text.point -> error) ref
val make_command : string Options.option_record
val set_compilation_buffer : Efuns.frame -> Efuns.buffer -> string -> unit
*)
