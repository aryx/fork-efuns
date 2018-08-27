
val compile : Efuns.action
val grep : Efuns.action

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

(* to setup as major mode variables in the different pl modes *)
val find_error_location_regexp : Str.regexp Store.var
val find_error_error_regexp: Str.regexp Store.var
(* to setup if find_error_gen is not good enough for you *)
val find_error : find_error_fun Store.var

val compile_find_makefile : bool Options.option_record

(*
val compilation_frame : (Efuns.frame * Text.point * string) option ref
val c_error_regexp : (string * Str.regexp) Options.option_record
val default_error : (Text.t -> Text.point -> error) ref
val make_command : string Options.option_record
val make_hist : string list ref
val set_compilation_buffer : Efuns.frame -> Efuns.buffer -> string -> unit
val grep_command : string Options.option_record
val grep_hist : string list ref
*)
