
type error = {
  err_filename : string;
  err_line : int;
  err_begin : int;
  err_end : int;
  err_msg : int;
}
type find_error_fun = Text.t -> Text.point -> error

val compile : find_error_fun -> Efuns.action
val grep : Efuns.action
val next_error : Efuns.action

val c_find_error : find_error_fun

val compile_find_makefile : bool Options.option_record

(*
val compilation_frame : (Efuns.frame * Text.point * string) option ref
val c_error_regexp : (string * Str.regexp) Options.option_record
val find_error : (Text.t -> Text.point -> error) Local.var
val default_error : (Text.t -> Text.point -> error) ref
val make_command : string Options.option_record
val make_hist : string list ref
val set_compilation_buffer : Efuns.frame -> Efuns.buffer -> string -> unit
val grep_command : string Options.option_record
val grep_hist : string list ref
*)
