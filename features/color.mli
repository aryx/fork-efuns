
(* helper to colorize a buffer *)
val color: Efuns.buffer -> Str.regexp -> bool (* strict *) -> Text.attribute ->
 unit

(* to be set by each major programming mode *)
val color_func: (Efuns.buffer -> Text.point -> Text.point -> unit) Var.t

val color_region: Efuns.action
val color_buffer: Efuns.action

(* to be used in 'install' of a mode *)
val color_buffer_buf: Efuns.buffer -> unit
