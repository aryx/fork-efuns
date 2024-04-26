
(* screen *)

val forward_screen : Efuns.action
val backward_screen : Efuns.action

val scroll_line : Efuns.frame -> int -> unit
val recenter : Efuns.action

val scroll_down: Efuns.action
val scroll_up: Efuns.action
val scroll_other_down: Efuns.action
val scroll_other_up: Efuns.action
