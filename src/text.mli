(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

exception ReadOnlyBuffer

type position = int
type delta = int

type direct
type point
type action
type t
type session
type attribute = int
  
type repr =
  { repr_line_pos: int;
    repr_line_len: int;
    mutable repr_attr: attribute;
    repr_charsize: int;
    repr_size: int;
    repr_pos: int }


type line =
  { 
    mutable position : direct;
    mutable representation : repr list;
    mutable modified : int; (* first modified position *)
    mutable repr_len : int;
    mutable repr_string : string;
    mutable line_hlt : int;
    mutable items : WX_text.item array;
  }


val dummy_line : line

val create : string -> t

val undo : t -> action * position * delta
val insert_at_end : t -> string -> unit
val insert_res : t -> point -> string -> position * int
val delete_res : t -> point -> delta -> position * string
val insert : t -> point -> string -> unit
val delete : t -> point -> delta -> unit

val direct_attr : attribute
val inverse_attr : attribute
val make_attr: int -> int -> int -> bool -> attribute
val set_attr : t -> point -> delta -> attribute -> unit
val unset_attr : t -> unit

val find_xy : t -> int -> int -> int -> int * int

val add_point : t -> point
val dup_point : t -> point -> point
val remove_point : t -> point -> unit
val distance : t -> point -> point -> delta
val get_position : t -> point -> position
val set_position : t -> point -> position -> unit
val goto_point : t -> point -> point -> unit

val read : in_channel -> t
val save : t -> out_channel -> unit
val get_char : t -> point -> char
val get_attr : t -> point -> attribute
val set_char_attr : t -> point -> attribute -> unit
val bmove_res : t -> point -> delta -> delta
val bmove : t -> point -> delta -> unit
val fmove_res : t -> point -> delta -> delta
val fmove : t -> point -> delta -> unit
val to_string : t -> string
val sub : t -> point -> delta -> string
val clear : t -> unit

val search_forward : t -> Str.regexp -> point -> delta
val search_forward_matched : t -> Str.regexp -> point -> string
val search_backward : t -> Str.regexp -> point -> delta
val search_forward_groups :
  t -> Str.regexp -> point -> int -> string array
val search_backward_groups :
  t -> Str.regexp -> point -> int -> string array
val replace_matched : t -> string -> string

val compute_representation : t -> string array -> int -> line

val point_to_eol : t -> point -> int
val point_to_bol : t -> point -> int
val point_to_eof : t -> point -> int
val point_to_bof : t -> point -> int
val move_res : t -> point -> int -> int
val move : t -> point -> int -> unit
val point_to_lof : t -> point -> int -> int
val point_to_lol : t -> point -> int -> int
val point_to_line : t -> point -> int -> unit


val point_line : t -> point -> int
val point_col : t -> point -> int
val goto_line : t -> point -> int -> unit

val version : t -> int
val nbre_lines : t -> int
val size : t -> int
val region : t -> point -> point -> string
val update : t -> string -> unit

val lexing : t -> point -> point -> Lexing.lexbuf
val start_session : t -> session
val commit_session : t -> session -> unit

val readonly : t -> bool
val toggle_readonly : t -> unit

val add_amount : int Options.option_record
val compare : t -> point -> point -> int

module TextTree :
  sig
    type 'a tree_desc
    and tree
    and redraw
    class t :
      WX_types.container ->
      tree tree_desc ->
      WX_types.base_attributes list ->
      object
      val mutable font : WX_types.font
      val id : int
      val mutable parent : WX_types.container
      val mutable redraw_area : redraw
      val s : WX_types.screen_struct
      val mutable szhints : WX_types.szhints
      val mutable text : tree tree_desc
      val w : WX_types.window
      val mutable widgets : WX_types.contained list
      method actions : (WX_types.event_desc * WX_types.handler) list
      method background : WX_types.color
      method click_type : WX_types.click
      method color_make : string -> bool -> WX_types.color
      method configure : WX_types.base_attributes list -> unit
      method contained : WX_types.contained
      method container : WX_types.container
      method cursor_make : WX_types.cursor_desc -> bool -> WX_types.cursor
      method default_font : WX_types.font
      method destroy : unit
      method display : Xtypes.display
      method draw_relief : unit
      method focus : unit
      method font_make : string -> bool -> WX_types.font
      method foreground : WX_types.color
      method geometry : Xtypes.geometry
      method getHilite : WX_types.color -> WX_types.color
      method getShadow : WX_types.color -> WX_types.color
      method global_color_make :
        WX_types.color_desc -> bool -> WX_types.color
      method handle_button : unit -> unit
      method handle_key : unit -> unit
      method height : int
      method hide : unit
      method id : string
      method inverse : unit
      method iter : (WX_types.contained -> unit) -> unit
      method iter_visible : (WX_types.contained -> unit) -> unit
      method name : string
      method normal : unit
      method parent : WX_types.container
      method pixmap_make : string * WX_types.pixmap_desc -> Xpm.pixmap
      method realize : unit
      method refresh : unit
      method reverse : bool
      method root_coordinates : int * int
      method screen : WX_types.screen_struct
      method set_parent : WX_types.container -> unit
      method set_text : tree tree_desc -> unit
      method show : unit
      method size_allocate : int -> int -> int -> int -> unit
      method size_request : WX_types.szhints
      method to_refresh : WX_types.refresh_widget
      method to_resize : WX_types.resize_widget
      method update : unit
      method update_size : unit
      method update_top_size : unit
      method wait_refresh :
        bool -> Xtypes.coord -> Xtypes.coord -> Xtypes.size -> Xtypes.size -> unit
      method wait_resize : unit
      method width : int
      method window : Xtypes.window
      method xevents : Xtypes.xevent -> unit
      end
  end

external id: t -> TextTree.tree TextTree.tree_desc = "%identity"