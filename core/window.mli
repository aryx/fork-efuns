(* see Efuns.window type *)
type t = Efuns.window

(* creation *)

val create :
  bool -> Efuns.window_up -> int -> int -> int -> int -> t
val create_at_top : int -> int -> int -> int -> t

(* getters/setters *)

val top : t -> Efuns.top_window

val prev : (Efuns.frame -> unit) -> t -> unit
val next : (Efuns.frame -> unit) -> t -> unit

(* iterator *)

val iter : (Efuns.frame -> unit) -> t -> unit

(* display *)

(*
val first : (Efuns.frame -> unit) -> t -> unit
val last : (Efuns.frame -> unit) -> t -> unit
*)
