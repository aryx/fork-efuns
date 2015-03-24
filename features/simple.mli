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

(* insertion *)

val insert_char : Efuns.frame -> char -> unit
val insert_string : Efuns.frame -> string -> unit
val insert_return : Efuns.frame -> unit

val self_insert_command : Efuns.frame -> unit
val insert_special_char : Efuns.frame -> unit

(* diff?*)
val insert_at_place : Efuns.frame -> char -> unit
val char_insert_command : char -> Efuns.frame -> unit

val electric_insert_space : Efuns.frame -> unit

(* deletion *)

val delete_char : Efuns.frame -> unit
val delete_backspace_char : Efuns.frame -> unit

val hungry_char : char -> bool
val hungry_electric_delete : Efuns.frame -> unit

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

val beginning_of_line : Efuns.frame -> unit
val end_of_line : Efuns.frame -> unit

val forward_line : Efuns.frame -> unit
val backward_line : Efuns.frame -> unit

(* paragraph *)

val backward_paragraph : Efuns.buffer -> Text.point -> unit
val forward_paragraph : Efuns.buffer -> Text.point -> unit

val fill_paragraph : Efuns.frame -> unit

(* screen *)

val forward_screen : Efuns.frame -> unit
val backward_screen : Efuns.frame -> unit

val scroll_line : Efuns.frame -> int -> unit
val recenter : Efuns.frame -> unit

(* file *)

val end_of_file : Efuns.frame -> unit
val begin_of_file : Efuns.frame -> unit


(* cut/copy/paste *)

val point_at_mark : Efuns.frame -> unit

val kill_string : string -> unit
val kill_text : Text.t -> Text.point -> Text.delta -> unit
val kill_end_of_line : Efuns.frame -> unit
val kill_eol : Efuns.buffer -> Text.point -> unit
val kill_bol : Efuns.buffer -> Text.point -> unit

val kill_size : int ref
val kill_max : int
val kill_ring : string array

val last_kill : (Text.t * Text.position) option ref
val last_insert : (Efuns.frame * Text.position * int * Text.delta) option ref

val insert_killed : Efuns.frame -> unit
val insert_next_killed : Efuns.frame -> unit

val kill_region : Efuns.frame -> unit

(* undo *)

val undo : Efuns.frame -> unit

(* attributes *)

val unset_attr : Efuns.frame -> unit

val color : Efuns.buffer -> Str.regexp -> bool -> Text.attribute -> unit

(* highlight *)

val highlighted : (Efuns.frame * Text.position * Text.position) option ref
val highlight_bit : int
val unhightlight_region :
  Efuns.buffer -> Text.position -> Text.position -> unit
val hightlight_region :
  Efuns.buffer -> Text.position -> Text.position -> unit
val highlighted_chars : (Efuns.buffer * Text.point * Text.attribute) list ref
val unhightlight : unit -> unit
val highlight : Efuns.frame -> unit

(* paren mode *)

val is_paren_end : char -> bool
val is_paren_begin : char -> bool

val highlight_paren : Efuns.frame -> unit

(* mouse *)

val mouse_set_frame : Efuns.frame -> unit

val mouse_drag_region : Efuns.frame -> unit
val mouse_yank_at_click : Efuns.frame -> unit
val mouse_save_then_kill : Efuns.frame -> unit

(* buffers *)

val buffer_list : Efuns.frame -> string list

val next_buffer : Efuns.buffer -> Efuns.buffer
val kill_buffer : Efuns.frame -> unit

(* indentation *)

val set_indent : Text.t -> Text.point -> int -> unit

(* structures *)

val insert_structure : string -> Efuns.frame -> unit
val install_structures :
  Efuns.buffer -> (Efuns.key list * string) list -> unit

(* parameters *)

type parameter =    (string * ((string -> Obj.t) * (Obj.t -> string) * 
      Obj.t Options.option_record))
val parameters_var : parameter list Local.var
val add_parameter :
  string -> (string -> 'a) -> ('a -> string) -> 'a Options.option_record -> unit

val add_option_parameter : 'a Options.option_record -> unit
val all_params : (parameter list * string list) option ref
val all_parameters : Efuns.frame -> 'a -> string list
(*
external id : 'a -> 'a = "%identity"
val add_string_parameter : Efuns.location -> string -> string ref -> unit
val add_int_parameter : Efuns.location -> string -> int ref -> unit
val add_float_parameter : Efuns.location -> string -> float ref -> unit
val add_bool_parameter : Efuns.location -> string -> bool ref -> unit
*)

(* misc *)

val format_to : Efuns.frame -> unit
val format_to_string : unit -> string ref

val to_frame : (Efuns.buffer -> Text.point -> 'a) -> Efuns.frame -> 'a
val htmlp : bool ref
val simplify : Text.t -> Text.point -> Text.point -> unit
val next_hole : Efuns.frame -> unit
val line_comment : string Local.var
val binding_option :
  ((Efuns.mod_ident * int) list * string) Options.option_class


(*
val single_char : string
*)
