
(* M-! *)
val shell_command : Efuns.action

type end_action = (Efuns.buffer -> int -> unit)

val system : 
  Common.dirname -> string -> string -> end_action -> 
  Efuns.buffer
val start_command : 
  Common.dirname -> string -> Efuns.window -> string -> end_action option -> 
  Efuns.frame

(* used by shell_mode *)
val open_process : Common.dirname -> string -> int * in_channel * out_channel

(*
val shell_hist : string list ref
*)
