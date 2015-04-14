
val hightlight_region :
  Efuns.buffer -> Text.position -> Text.position -> unit
val unhightlight_region :
  Efuns.buffer -> Text.position -> Text.position -> unit

val highlighted : (Efuns.frame * Text.position * Text.position) option ref
val highlight_bit : int
val highlighted_chars : (Efuns.buffer * Text.point * Text.attribute) list ref
val unhightlight : unit -> unit

val highlight : Efuns.action
