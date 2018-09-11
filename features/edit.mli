(* insertion *)

val insert_char : Efuns.frame -> char -> unit
(* when in overwrite mode *)
val insert_at_place : Efuns.frame -> char -> unit
val insert_string : Efuns.frame -> string -> unit

val insert_return : Efuns.action
val self_insert_command : Efuns.action

(* ? *)
val char_insert_command : char -> Efuns.action

(* deletion *)

val delete_char : Efuns.action
val delete_backspace_char : Efuns.action

(* characters *)

val transpose_chars : Efuns.buffer -> Text.point -> unit

(* words *)

val delete_backward_word : Efuns.buffer -> Text.point -> unit
val delete_forward_word : Efuns.buffer -> Text.point -> unit

val on_word : Efuns.buffer -> Text.point -> (string -> string) -> unit
val transpose_words : Efuns.buffer -> Text.point -> unit

(* overwrite *)
val overwrite_mode: Efuns.minor_mode

val toggle_overwrite_mode: Efuns.action

(* undo *)

val undo : Efuns.action
