
val hightlight_region :
  Efuns.buffer -> Text.position -> Text.position -> unit
val unhightlight_region :
  Efuns.buffer -> Text.position -> Text.position -> unit

(* highlight between mark and point *)
(*
val highlight : Efuns.action
val unhightlight : Efuns.action
*)
(* see Text.make_attr *)
val highlighted_chars : (Efuns.buffer * Text.point * Text.attribute) list ref

(*
val highlighted : (Efuns.frame * Text.position * Text.position) option ref
*)
