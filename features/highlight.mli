
val highlight_region :
  Efuns.buffer -> Text.position -> Text.position -> unit
val unhighlight_region :
  Efuns.buffer -> Text.position -> Text.position -> unit

(* see Text.make_attr *)
val highlighted_chars : (Efuns.buffer * Text.point * Text.attribute) list ref
val highlighted : (Efuns.frame * Text.position * Text.position) option ref

val unhighlight_hook:
  (Text.t * Text.position * Text.position -> unit) list Store.var

(* this will be called before each keystroke and unhighlight everything
 * that was previously highlighted
 *)
val unhighlight : Efuns.action
