
(* to be set by each major programming mode for the index_xxx below to work *)
val indent_func: (Efuns.buffer -> Text.point -> Text.point -> unit) Var.t

val indent_region: Efuns.action
val indent_phrase: Efuns.action
val indent_buffer: Efuns.action


(* helpers to build an indent_func *)
val set_indent : Text.t -> Text.point -> int -> unit
