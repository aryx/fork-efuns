
(* The text ! *)
type t

(* creation *)

val create : string -> t
val read : in_channel -> t

(* load/save/kill *)

val save : t -> out_channel -> unit

(* content getters/setters *)

val to_string : t -> string
val clear : t -> unit
val update : t -> string -> unit

val size : t -> int (* can be used also as a position *)
val nbr_lines : t -> int

(* positions, points, line x col coordinates *)

type position = int

(* should be abstract, but can be useful to have public for ocamldebug *)
(*
type point
type position2 (* for line *)
*)
type position2 = int
type point = {
    mutable pos : position2;
    mutable line : int;
}

type coord = {
  c_col: int;
  c_line: int; (* 0-based *)
}

(* point creation/deletion *)

val new_point : t -> point
val dup_point : t -> point -> point
val with_dup_point: t -> point -> (point -> 'a) -> 'a
val with_new_point: t -> (point -> 'a) -> 'a

val remove_point : t -> point -> unit

(* point getters/setters *)

val get_position : t -> point -> position
val set_position : t -> point -> position -> unit
val goto_point : t -> point -> point -> unit

(* Line x col *)

val point_line : t -> point -> int (* 0-based *)
val point_col : t -> point -> int
val point_coord: t -> point -> coord

val goto_line : t -> point -> int -> unit

(* point begin/end line/file distance to point *)

val point_to_bol : t -> point -> int
val point_to_eol : t -> point -> int
val point_to_bof : t -> point -> int
val point_to_eof : t -> point -> int
(* ?? *)
val point_to_lof : t -> point -> int -> int
val point_to_lol : t -> point -> int -> int
(* ?? *)
val point_to_line : t -> point -> int -> unit

(* move *)

val move : t -> point -> int -> unit
val move_res : t -> point -> int -> int

(* relative move *)

type delta = int

val distance : t -> point -> point -> delta

val bmove : t -> point -> delta -> unit
val fmove : t -> point -> delta -> unit
val bmove_res : t -> point -> delta -> delta
val fmove_res : t -> point -> delta -> delta

val iter: t -> point -> int -> (point -> unit) -> unit

(* access *)

val get_char : t -> point -> char
val region : t -> point -> point -> string
val sub : t -> point -> delta -> string

(* insertion/deletion *)

val insert : t -> point -> string -> unit
val delete : t -> point -> delta -> unit
val insert_res : t -> point -> string -> position * int
val delete_res : t -> point -> delta -> position * string

val insert_at_end : t -> string -> unit

(* undo *)

type action
val undo : t -> action * position * delta

type session
val start_session : t -> session
val commit_session : t -> session -> unit
val with_session: (unit -> 'a) -> t -> 'a

(* attributes *)

type attribute = int
val direct_attr : attribute
val inverse_attr : attribute
(* for use by other files, e.g. paren_mode.ml *)
val highlight_bit : int

val make_attr: int -> int -> int -> bool -> attribute
val get_attr : t -> point -> attribute
val set_attr : t -> point -> attribute -> unit

val set_attrs : t -> point -> delta -> attribute -> unit
val unset_attrs : t -> unit

(* search *)

(* the point is modified as a side effect to point to the start of the
 * matched regexp.
 *)
val search_forward : t -> Str.regexp -> point -> delta
val search_backward : t -> Str.regexp -> point -> delta

val search_forward_groups :
  t -> Str.regexp -> point -> int -> string array
val search_backward_groups :
  t -> Str.regexp -> point -> int -> string array

val search_forward_matched : t -> Str.regexp -> point -> string
val replace_matched : t -> string -> string

(* misc *)

type version = int
val version : t -> version

val readonly : t -> bool
val toggle_readonly : t -> unit

val lexing : t -> point -> point -> Lexing.lexbuf
val add_amount : int Options.t
val compare : t -> point -> point -> int
(*val find_xy : t -> int -> int -> int -> coord *)

(* line (mostly for display) *)

type line =
  { 
    mutable position : position2;

    mutable boxes : box list;
    mutable repr_string : bytes;
    mutable repr_len : int;
    mutable line_modified : bool;
  }

  and box = { 
    box_col : int;        (* col of box *)
    box_len : int;        (* len of box in Text.t string *)
    mutable box_attr : int;    (* common attribute *)

    box_charsize : int; (* common size *)
    box_size : int;
    box_pos_repr : int;  (* pos of repbox in representation string *)
   }
  
val dummy_line : line

(* display *)
type charreprs = string array

val compute_representation : t -> charreprs -> int -> line

(* 
move_point_to_pos: t -> point -> position2 -> unit 
find_line_of_pos: t -> position2 -> int
*)
