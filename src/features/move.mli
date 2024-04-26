(* characters *)

val move_backward : Efuns.frame -> Text.delta -> unit
val move_forward : Efuns.frame -> Text.delta -> unit

(* words *)

val in_next_word : Text.t -> Text.point -> bool array -> unit
val in_prev_word : Text.t -> Text.point -> bool array -> unit
val to_begin_of_word : Text.t -> Text.point -> bool array -> unit
val to_end_of_word : Text.t -> Text.point -> bool array -> unit

val backward_word : Efuns.buffer -> Text.point -> unit
val forward_word : Efuns.buffer -> Text.point -> unit

val beginning_of_word : Efuns.buffer -> Text.point -> string
val end_of_word : Efuns.buffer -> Text.point -> string

val current_word : Efuns.buffer -> Text.point -> string

(* line *)

val line_size : Efuns.frame -> int
val begin_to_point : Efuns.frame -> int
val point_to_end : Efuns.frame -> int

val beginning_of_line : Efuns.action
val end_of_line : Efuns.action

val forward_line : Efuns.action
val backward_line : Efuns.action

(* paragraph *)

val backward_paragraph : Efuns.buffer -> Text.point -> unit
val forward_paragraph : Efuns.buffer -> Text.point -> unit

(* file *)

val end_of_file : Efuns.action
val begin_of_file : Efuns.action

(* mark *)
val point_at_mark : Efuns.action

(* history navigation *)
val goto_last_saved_pos: Efuns.action
val save_current_pos: Efuns.frame -> unit (* not an action *)

