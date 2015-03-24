
(* M-! *)
val shell_command : Efuns.action

val system : 
  string -> string -> (Efuns.buffer -> int -> unit) -> Efuns.buffer
val start_command : 
  string -> Efuns.window -> string -> Efuns.frame

(* used by shell_mode *)
val open_process : string -> int * in_channel * out_channel

(*
val shell_hist : string list ref
*)
