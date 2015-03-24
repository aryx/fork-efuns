
val create_bindings : unit -> Efuns.buffer

val exec_interactive :
  (string * ((Efuns.frame -> unit) * Efuns.keys option)) list ->
  Efuns.frame -> string -> unit
val call_interactive : Efuns.frame -> unit

val buf_interactives :
  Efuns.buffer -> (string * (Efuns.action * Efuns.prefix option)) list

(*
val meta_hist : string list ref
*)
