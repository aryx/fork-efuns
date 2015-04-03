(* see Efuns.window type *)

(* creation *)

val create :
  bool -> Efuns.window_up -> int -> int -> int -> int -> Efuns.window
val create_at_top : int -> int -> int -> int -> Efuns.window

(* getters/setters *)

val top : Efuns.window -> Efuns.top_window

val prev : (Efuns.frame -> unit) -> Efuns.window -> unit
val next : (Efuns.frame -> unit) -> Efuns.window -> unit

(* iterator *)

val iter : (Efuns.frame -> unit) -> Efuns.window -> unit

(* display *)

(*
val first : (Efuns.frame -> unit) -> Efuns.window -> unit
val last : (Efuns.frame -> unit) -> Efuns.window -> unit
*)
