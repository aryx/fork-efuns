
(* creation *)

val create : Efuns.frame -> Efuns.map -> string -> Efuns.frame

val create_return :
  Efuns.frame ->
  Efuns.map ->
  string -> string -> (Efuns.frame -> string -> unit) -> Efuns.frame

(* kill *)

val kill : Efuns.frame -> Efuns.frame -> unit

(* misc *)

val update_request : Efuns.frame -> string -> unit

(*
val charreprs : string array
val buf_create : Text.t -> Efuns.map -> Efuns.buffer
val return :
  (Efuns.frame -> string -> 'a) -> Efuns.frame -> Efuns.frame -> 'a
*)
