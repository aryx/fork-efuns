
(* insertion *)

val insert_char : Efuns.frame -> char -> unit
val insert_string : Efuns.frame -> string -> unit

val insert_return : Efuns.action
val self_insert_command : Efuns.action
val insert_special_char : Efuns.action

(* diff?*)
val insert_at_place : Efuns.frame -> char -> unit
val char_insert_command : char -> Efuns.action

val electric_insert_space : Efuns.action

(* deletion *)

val delete_char : Efuns.action
val delete_backspace_char : Efuns.action

val hungry_char : char -> bool
val hungry_electric_delete : Efuns.action

(* characters *)

val move_backward : Efuns.frame -> Text.delta -> unit
val move_forward : Efuns.frame -> Text.delta -> unit

val transpose_chars : Efuns.buffer -> Text.point -> unit

val previous_char : Efuns.frame -> char

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

val delete_backward_word : Efuns.buffer -> Text.point -> unit
val delete_forward_word : Efuns.buffer -> Text.point -> unit

val on_word : Efuns.buffer -> Text.point -> (string -> string) -> unit
val transpose_words : Efuns.buffer -> Text.point -> unit

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

val fill_paragraph : Efuns.action
val line_comment : string Local.var

(* screen *)

val forward_screen : Efuns.action
val backward_screen : Efuns.action

val scroll_line : Efuns.frame -> int -> unit
val recenter : Efuns.action

val scroll_down: Efuns.action
val scroll_up: Efuns.action
val scroll_other_down: Efuns.action
val scroll_other_up: Efuns.action

(* file *)

val end_of_file : Efuns.action
val begin_of_file : Efuns.action


(* cut/copy/paste *)

val point_at_mark : Efuns.action

val kill_string : string -> unit
val kill_text : Text.t -> Text.point -> Text.delta -> unit
val kill_end_of_line : Efuns.action
val kill_eol : Efuns.buffer -> Text.point -> unit
val kill_bol : Efuns.buffer -> Text.point -> unit

val kill_size : int ref
val kill_max : int
val kill_ring : string array

val last_kill : (Text.t * Text.position) option ref
val last_insert : (Efuns.frame * Text.position * int * Text.delta) option ref

val insert_killed : Efuns.action
val insert_next_killed : Efuns.action

val kill_region : Efuns.action

(* undo *)

val undo : Efuns.action

(* attributes *)

val unset_attr : Efuns.action

val color : Efuns.buffer -> Str.regexp -> bool -> Text.attribute -> unit

(* misc *)

val to_frame : (Efuns.buffer -> Text.point -> 'a) -> Efuns.frame -> 'a

val simplify : Text.t -> Text.point -> Text.point -> unit

val binding_option :
  ((Efuns.mod_ident * int) list * string) Options.option_class


(*
val single_char : string
*)
