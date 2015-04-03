
val highlighted : (Efuns.frame * Text.position * Text.position) option ref
val highlight_bit : int
val unhightlight_region :
  Efuns.buffer -> Text.position -> Text.position -> unit
val hightlight_region :
  Efuns.buffer -> Text.position -> Text.position -> unit
val highlighted_chars : (Efuns.buffer * Text.point * Text.attribute) list ref
val unhightlight : unit -> unit
val highlight : Efuns.action
