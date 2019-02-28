
val hightlight_region :
  Efuns.buffer -> Text.position -> Text.position -> unit
val unhightlight_region :
  Efuns.buffer -> Text.position -> Text.position -> unit

(* highlight between mark and point *)
(* dead for now, but if enable mouse selection then shoud be live again *)
val highlight : Efuns.action

(* see Text.make_attr *)
val highlighted_chars : (Efuns.buffer * Text.point * Text.attribute) list ref
(*val highlighted : (Efuns.frame * Text.position * Text.position) option ref*)

(* this will be called before each keystroke and unhighlight everything
 * that was previously highlighted
 *)
val unhighlight : Efuns.action
