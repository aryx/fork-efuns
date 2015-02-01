(***********************************************************************)
(*                                                                     *)
(*                             Efuns                                   *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

val binding_option :
  ((Efuns.mod_ident * int) list * string) Options.option_class
val insert_string : Efuns.frame -> string -> unit
val single_char : string
val insert_char : Efuns.frame -> char -> unit
val insert_return : Efuns.frame -> unit
val previous_char : Efuns.frame -> char
val unset_attr : Efuns.frame -> unit
val insert_at_place : Efuns.frame -> char -> unit
val self_insert_command : Efuns.frame -> unit
val char_insert_command : char -> Efuns.frame -> unit
val move_backward : Efuns.frame -> Text.delta -> unit
val move_forward : Efuns.frame -> Text.delta -> unit
val begin_to_point : Efuns.frame -> int
val point_to_end : Efuns.frame -> int
val line_size : Efuns.frame -> int
val beginning_of_line : Efuns.frame -> unit
val end_of_line : Efuns.frame -> unit
val forward_line : Efuns.frame -> unit
val backward_line : Efuns.frame -> unit
val kill_size : int ref
val kill_max : int
val kill_ring : string array
val last_kill : (Text.t * Text.position) option ref
val last_insert : (Efuns.frame * Text.position * int * Text.delta) option ref
val kill_string : string -> unit
val kill_text : Text.t -> Text.point -> Text.delta -> unit
val kill_end_of_line : Efuns.frame -> unit
val kill_eol : Efuns.buffer -> Text.point -> unit
val kill_bol : Efuns.buffer -> Text.point -> unit
val insert_killed : Efuns.frame -> unit
val insert_next_killed : Efuns.frame -> unit
val format_to : Efuns.frame -> unit
val format_to_string : unit -> string ref
val in_next_word : Text.t -> Text.point -> bool array -> unit
val in_prev_word : Text.t -> Text.point -> bool array -> unit
val to_begin_of_word : Text.t -> Text.point -> bool array -> unit
val to_end_of_word : Text.t -> Text.point -> bool array -> unit
val to_frame : (Efuns.buffer -> Text.point -> 'a) -> Efuns.frame -> 'a
val backward_word : Efuns.buffer -> Text.point -> unit
val forward_word : Efuns.buffer -> Text.point -> unit
val beginning_of_word : Efuns.buffer -> Text.point -> string
val end_of_word : Efuns.buffer -> Text.point -> string
val current_word : Efuns.buffer -> Text.point -> string
val current_word : Efuns.buffer -> Text.point -> string
val dirname : Efuns.frame -> string -> string
val buffer_list : Efuns.frame -> string list
val delete_char : Efuns.frame -> unit
val delete_backspace_char : Efuns.frame -> unit
val hungry_char : char -> bool
val hungry_electric_delete : Efuns.frame -> unit
val forward_screen : Efuns.frame -> unit
val backward_screen : Efuns.frame -> unit
val scroll_line : Efuns.frame -> int -> unit
val recenter : Efuns.frame -> unit
val end_of_file : Efuns.frame -> unit
val begin_of_file : Efuns.frame -> unit
val delete_backward_word : Efuns.buffer -> Text.point -> unit
val delete_forward_word : Efuns.buffer -> Text.point -> unit
val undo : Efuns.frame -> unit
val kill_region : Efuns.frame -> unit
val mouse_set_frame : Efuns.frame -> unit
val highlighted : (Efuns.frame * Text.position * Text.position) option ref
val highlight_bit : int
val unhightlight_region :
  Efuns.buffer -> Text.position -> Text.position -> unit
val hightlight_region :
  Efuns.buffer -> Text.position -> Text.position -> unit
val highlighted_chars : (Efuns.buffer * Text.point * Text.attribute) list ref
val unhightlight : 'a -> unit
val highlight : Efuns.frame -> unit
val htmlp : bool ref
val is_paren_end : char -> bool
val is_paren_begin : char -> bool
val highlight_paren : Efuns.frame -> unit
val mouse_drag_region : Efuns.frame -> unit
val mouse_yank_at_click : Efuns.frame -> unit
val mouse_save_then_kill : Efuns.frame -> unit
val next_buffer : Efuns.location -> Efuns.buffer -> Efuns.buffer
val kill_buffer : Efuns.frame -> unit
val color : Efuns.buffer -> Str.regexp -> bool -> Text.attribute -> unit
val point_at_mark : Efuns.frame -> unit
val on_word : Efuns.buffer -> Text.point -> (string -> string) -> unit
val transpose_words : Efuns.buffer -> Text.point -> unit
val transpose_chars : Efuns.buffer -> Text.point -> unit
val backward_paragraph : Efuns.buffer -> Text.point -> unit
val forward_paragraph : Efuns.buffer -> Text.point -> unit
val electric_insert_space : Efuns.frame -> unit
val simplify : Text.t -> Text.point -> Text.point -> unit
val fill_paragraph : Efuns.frame -> unit
val set_indent : Text.t -> Text.point -> int -> unit
val insert_special_char : Efuns.frame -> unit
val next_hole : Efuns.frame -> unit
val insert_structure : string -> Efuns.frame -> unit
val install_structures :
  Efuns.buffer -> (Efuns.key list * string) list -> unit
type parameter =    (string * ((string -> Obj.t) * (Obj.t -> string) * 
      Obj.t Options.option_record))
val parameters_var : parameter list Local.var
val add_parameter :
  Efuns.location ->
  string -> (string -> 'a) -> ('a -> string) -> 'a Options.option_record -> unit
  (*
external id : 'a -> 'a = "%identity"
val add_string_parameter : Efuns.location -> string -> string ref -> unit
val add_int_parameter : Efuns.location -> string -> int ref -> unit
val add_float_parameter : Efuns.location -> string -> float ref -> unit
  val add_bool_parameter : Efuns.location -> string -> bool ref -> unit
  *)
val add_option_parameter : Efuns.location -> 'a Options.option_record -> unit
val all_params : (parameter list * string list) option ref
val all_parameters : Efuns.frame -> 'a -> string list
val line_comment : string Local.var