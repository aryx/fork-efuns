
(* M-! *)
val shell_command : Efuns.action

type end_action = (Efuns.buffer -> int -> unit)

val system : 
  Common.filename (* a dir *) -> string -> string -> end_action -> 
  Efuns.buffer
val start_command : 
  Common.filename (* a dir *) -> string -> Efuns.window -> string -> end_action option -> 
  Efuns.frame

(* used by shell_mode *)
val open_process : Common.filename (* a dir *) -> string -> int * in_channel * out_channel

(*
val shell_hist : string list ref
*)
