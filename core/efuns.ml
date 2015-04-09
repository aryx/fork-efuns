(*s: core/efuns.ml *)
(*s: copyright header2 *)
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
(*e: copyright header2 *)
(* Useful types for Efuns *)

(*s: exception Efuns.UnboundKey *)
exception UnboundKey
(*e: exception Efuns.UnboundKey *)

(*************************************************************************)
(* Keymaps and actions *)
(*************************************************************************)

(*s: type Efuns.map *)
type map =
  { 
    (* 256 array, one character simple key = one action *)
    char_map : binding array;
    (* complex key, possible sub maps *)
    mutable complex_bindings : (key * binding) list;

    (*s: [[Efuns.map]] other fields *)
    mutable interactives : (string * (action * prefix option)) list;
    (*e: [[Efuns.map]] other fields *)
  } 
(*e: type Efuns.map *)
(*s: type Efuns.keySym *)
and keySym = int
(*e: type Efuns.keySym *)
(*s: type Efuns.key *)
and key = mod_ident * keySym
(*e: type Efuns.key *)

(*s: type Efuns.action *)
and action = frame -> unit
(*e: type Efuns.action *)

and action_name = string

(*s: type Efuns.generic_action *)
and generic_action =
| FrameAction of action
| BufferAction of (buffer -> unit)
(*e: type Efuns.generic_action *)

(*s: type Efuns.mod_ident *)
and mod_ident = 
  NormalMap
| ControlMap
| MetaMap
| ControlMetaMap
(*e: type Efuns.mod_ident *)

and keys = key list

(*s: type Efuns.prefix *)
and prefix = key list
(*e: type Efuns.prefix *)

(*s: type Efuns.binding *)
and binding = 
  Function of action
| Prefix of map
| Unbound
(*e: type Efuns.binding *)

(*************************************************************************)
(* Buffers *)
(*************************************************************************)

(*s: type Efuns.buffer *)
(* a buffer containing a file in Text.t *)
and buffer =
  { 
    mutable buf_text : Text.t;

    (* should be unique; can be used as a primary key in location.loc_buffers*)
    mutable buf_name : string;
    mutable buf_filename : string option;

    (*s: [[Efuns.buffer]] current position fields *)
    mutable buf_point : Text.point;
    mutable buf_start : Text.point;
    (*e: [[Efuns.buffer]] current position fields *)
    (*s: [[Efuns.buffer]] history fields *)
    mutable buf_modified : int; (* version? *)
    (*x: [[Efuns.buffer]] history fields *)
    mutable buf_last_saved : Text.version;
    (*e: [[Efuns.buffer]] history fields *)
    (*s: [[Efuns.buffer]] other fields *)
    mutable buf_shared : int; (* number of frames for that buffer *)
    (*x: [[Efuns.buffer]] other fields *)
    buf_map : map;
    (*x: [[Efuns.buffer]] other fields *)
    mutable buf_map_partial : bool;
    (*x: [[Efuns.buffer]] other fields *)
    mutable buf_vars : Local.vars;
    (*x: [[Efuns.buffer]] other fields *)
    mutable buf_charreprs : Text.charreprs; (* 256 array *)
    (*x: [[Efuns.buffer]] other fields *)
    mutable buf_syntax_table : bool array;
    (*x: [[Efuns.buffer]] other fields *)
    mutable buf_mark : Text.point option;
    (*x: [[Efuns.buffer]] other fields *)
    mutable buf_major_mode : major_mode;
    (*x: [[Efuns.buffer]] other fields *)
    mutable buf_minor_modes : minor_mode list;
    (*x: [[Efuns.buffer]] other fields *)
    mutable buf_finalizers : (unit -> unit) list;
    (*x: [[Efuns.buffer]] other fields *)
    mutable buf_sync : bool;
    (*e: [[Efuns.buffer]] other fields *)
  } 
(*e: type Efuns.buffer *)

(*s: type Efuns.major_mode *)
and major_mode = {
    maj_name : string;
    maj_map : map;

    mutable maj_hooks : (buffer -> unit) list;
    mutable maj_vars : Local.vars;
  }
(*e: type Efuns.major_mode *)

(*s: type Efuns.minor_mode *)
and minor_mode = {
    min_name : string;
    min_map : map;

    mutable min_hooks : (buffer -> unit) list;
    mutable min_vars : Local.vars;
  }
(*e: type Efuns.minor_mode *)

(*************************************************************************)
(* Frames *)
(*************************************************************************)
  
(*s: type Efuns.frame *)
(* a frame: a view of a buffer for a window *)
and frame  =
  {
    mutable frm_buffer : buffer;

    mutable frm_xpos : int;
    mutable frm_ypos : int;
    
    mutable frm_width : int;
    mutable frm_height : int; (* including status line *)

    (*s: [[Efuns.frame]] current position fields *)
    (* insert point *)
    mutable frm_point : Text.point; 
    (*x: [[Efuns.frame]] current position fields *)
    (* first point of the first buffer-line on screen *)
    mutable frm_start : Text.point;
    (* last point on screen, -1 if modified *)
    mutable frm_end : Text.point;
    (*e: [[Efuns.frame]] current position fields *)
    (*s: [[Efuns.frame]] window fields *)
    mutable frm_window : window;
    (*e: [[Efuns.frame]] window fields *)
    (*s: [[Efuns.frame]] decoration fields *)
    (* 0 for no scrollbar, 2 for scrollbar *)
    mutable frm_has_scrollbar : int;
    (*x: [[Efuns.frame]] decoration fields *)
    (* 0 for minibuffer, 1 for normal frame *)
    mutable frm_has_status_line : int;
    (* Some for minibuffer, None for normal frame *)
    mutable frm_mini_buffer : string option;
    (*e: [[Efuns.frame]] decoration fields *)
    (*s: [[Efuns.frame]] status field *)
    mutable frm_status : status;    
    (*e: [[Efuns.frame]] status field *)
    (*s: [[Efuns.frame]] history fields *)
    mutable frm_last_text_updated : int;
    mutable frm_last_buf_updated : int;
    (*e: [[Efuns.frame]] history fields *)
    (*s: [[Efuns.frame]] other fields *)
    mutable frm_redraw : bool;    
    (*x: [[Efuns.frame]] other fields *)
    (* where do we put a \ for overflowing lines *)
    mutable frm_cutline : int; (* max_int for no, else length *)
    (*x: [[Efuns.frame]] other fields *)
    mutable frm_table : frm_line array;
    (*x: [[Efuns.frame]] other fields *)
    mutable frm_cursor_x : int;
    mutable frm_cursor_y : int;

    mutable frm_cursor : string;
    mutable frm_cursor_attr : Text.attribute;
    (*x: [[Efuns.frame]] other fields *)
    (* ?? *)
    mutable frm_x_offset : int;
    (* offset(+/-) of screen-lines after frm_start *)
    mutable frm_y_offset : int;
    (*x: [[Efuns.frame]] other fields *)
    mutable frm_force_start : bool;
    (*x: [[Efuns.frame]] other fields *)
    mutable frm_prefix : key list;
    (*x: [[Efuns.frame]] other fields *)
    mutable frm_last_action : action;
    (*x: [[Efuns.frame]] other fields *)
    mutable frm_killed : bool;
    (*e: [[Efuns.frame]] other fields *)
  } 
(*e: type Efuns.frame *)

(*s: type Efuns.status_info *)
and status_info =
| StatName
| StatFile
| StatLine
| StatCol
| StatModified
| StatMode
(*e: type Efuns.status_info *)

(*s: type Efuns.status *)
and status =
  { 
    mutable stat_name : string;
    mutable stat_file : string;
    mutable stat_line : int;
    mutable stat_col : int;

    mutable status_modified : bool;
    mutable stat_modified : bool;
    (*s: [[Efuns.status]] other fields *)
    (* the pair of int is a:   pos * max_len *)
    mutable status_format : (status_info * (int * int)) list;
    (*x: [[Efuns.status]] other fields *)
    (* the string! --- ... --- *)
    mutable status_string : string;
    (*x: [[Efuns.status]] other fields *)
    mutable stat_mode : major_mode;
    (*x: [[Efuns.status]] other fields *)
    mutable stat_modes : minor_mode list;
    (*e: [[Efuns.status]] other fields *)
  }
(*e: type Efuns.status *)

(*s: type Efuns.line_repr *)
and frm_line =
  { 
    mutable frm_text_line : Text.line;

   (* sorted normally, head = first box in line *)
   mutable frmline_boxes : Text.box list; 
   mutable first_box_extra_offset : int;

    (*s: [[Efuns.frm_line]] other fields *)
    mutable lineidx_in_text : int;
    (*x: [[Efuns.frm_line]] other fields *)
    (* previous values, so can check if the line changed *)
    mutable repr_prev_offset : int;
    mutable prev_frmline_boxes : Text.box list;
    (*e: [[Efuns.frm_line]] other fields *)
  } 
(*e: type Efuns.line_repr *)

(*************************************************************************)
(* Windows *)
(*************************************************************************)

(*s: type Efuns.top_window *)
(* an xterm: a window containing some frames *)
and top_window = 
  { 
    mutable top_width : int;
    mutable top_height : int; (* including minibuffer line *)

    mutable window : window;

    (*s: [[Efuns.top_window]] other fields *)
    mutable graphics : Xdraw.graphics_backend option;
    (*x: [[Efuns.top_window]] other fields *)
    mutable top_active_frame : frame;
    (*x: [[Efuns.top_window]] other fields *)
    mutable top_name : string;
    (*x: [[Efuns.top_window]] other fields *)
    mutable top_mini_buffers : frame list;
    (*x: [[Efuns.top_window]] other fields *)
    mutable top_second_cursor : frame option;
    (*e: [[Efuns.top_window]] other fields *)
  } 
(*e: type Efuns.top_window *)

(*s: type Efuns.window *)
and window =
  { 
    mutable win_xpos : int;
    mutable win_ypos : int;

    mutable win_width : int;
    mutable win_height : int; (* including status line *)

    (*s: [[Efuns.window]] other fields *)
    mutable win_down : window_down;
    mutable win_up : window_up;
    (*x: [[Efuns.window]] other fields *)
    mutable win_mini : bool;
    (*e: [[Efuns.window]] other fields *)
  } 
(*e: type Efuns.window *)

(*s: type Efuns.window_up *)
and window_up =
  Window of window
| TopWindow of top_window
(*e: type Efuns.window_up *)

(*s: type Efuns.window_down *)
and window_down =
| WFrame of frame
| NoFrame of unit

| HComb of window * window 
| VComb of window * window
(*e: type Efuns.window_down *)

(*************************************************************************)
(* The world *)
(*************************************************************************)

(*s: type Efuns.location *)
type location =
  { 
    (* key is buffer name (made unique via get_name()) *)
    mutable loc_buffers : (string, buffer) Hashtbl.t;
    (* key is filename (should be unique? use realpath?) *)
    mutable loc_files : (string, buffer) Hashtbl.t;

    (* list when have one efuns running multiple top windows *)
    mutable top_windows : top_window list;

    (* pwd of efuns when started *)
    mutable loc_dirname : string;

    (* general look, configurable via -xxx command line options or .efunsrc 
     * (dimension type is in characters)
     *)
    mutable loc_width : int;
    mutable loc_height : int;

    mutable loc_fg : string;
    mutable loc_bg : string;
    mutable loc_font : string;

    (*s: [[Efuns.location]] other fields *)
    loc_map : map;
    (*x: [[Efuns.location]] other fields *)
    loc_vars : Local.vars;
    (*x: [[Efuns.location]] other fields *)
    loc_colors : (string,int) Hashtbl.t;
    loc_colors_names : string array;
    mutable loc_colors_n : int;
    (*x: [[Efuns.location]] other fields *)
    loc_fonts : (string,int) Hashtbl.t;
    loc_fonts_names : string array;
    mutable loc_fonts_n : int;
    (*x: [[Efuns.location]] other fields *)
    loc_mutex : Mutex.t;
    (*e: [[Efuns.location]] other fields *)
  } 
(*e: type Efuns.location *)

(*************************************************************************)
(* Misc *)
(*************************************************************************)

(* used by frame, so can't be in top_window.ml *)
(*s: function Efuns.backend *)
let backend top_window =
  match top_window.graphics with
    None -> raise Not_found
  | Some x -> x
(*e: function Efuns.backend *)

(*s: type Efuns.sens *)
type sens = 
| Backward
| Forward
(*e: type Efuns.sens *)
(*s: type Efuns.to_regexp *)
type to_regexp =
  Regexp
| RegexpString
(*e: type Efuns.to_regexp *)

(*s: function Efuns.string_to_regex *)
let string_to_regex s = s, Str.regexp s
(*e: function Efuns.string_to_regex *)

open Options    
(*s: constant Efuns.regexp_option *)
let regexp_option = define_option_class "Regexp" 
    (fun v -> match v with
        Value s ->  string_to_regex s | _ -> raise Not_found)
  (fun (s,_r) -> Value s)
(*e: constant Efuns.regexp_option *)
(*e: core/efuns.ml *)
