
(* M-x *)
val call_interactive : Efuns.action

val exec_interactive :
  (string * (Efuns.action * Efuns.keys option)) list ->
  Efuns.frame -> string -> unit

val buf_interactives :
  Efuns.buffer -> (string * (Efuns.action * Efuns.prefix option)) list

(* help for user *)
val create_bindings : unit -> Efuns.buffer

(*
val meta_hist : string list ref
*)
