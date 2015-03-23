
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

val backend : Efuns.top_window -> Xdraw.graphics_backend

(* why here?? *)
val get_color : string -> int
val get_font : string -> int

(*
val first : (Efuns.frame -> unit) -> Efuns.window -> unit
val last : (Efuns.frame -> unit) -> Efuns.window -> unit
*)
