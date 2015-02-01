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

(* Useful types for Efuns *)

(*************************************************************************)
               (*      Types      *)
(*************************************************************************)

open Utils
open Obj
open Local
  
exception UnboundKey

type map =
  { char_map : binding array;
    mutable complex_bindings : (key * binding) list;
    mutable interactives : (string * (action * prefix option)) list;
  } 

and key = mod_ident * Xtypes.keySym

and action = frame -> unit

and generic_action =
  BufferAction of (buffer -> unit)
| FrameAction of (frame -> unit)

and mod_ident = 
  NormalMap
| ControlMap
| MetaMap
| ControlMetaMap

and prefix = key list

and binding = 
  Function of action
| Prefix of map
| Unbound

(* a buffer containing a file in Text.t *)
and buffer =
  { 
    mutable buf_text : Text.t;
    mutable buf_modified : int;
    
    mutable buf_name : string;
    mutable buf_filename : string option;
    mutable buf_last_saved : int;
    
    mutable buf_history : (int * Text.action) list;
    
    mutable buf_charreprs : string array;
    mutable buf_syntax_table : bool array;
    
    mutable buf_map_partial : bool;
    buf_map : map;
    
    mutable buf_sync : bool;
    mutable buf_mark : Text.point option;
    mutable buf_point : Text.point;
    mutable buf_start : Text.point;
    mutable buf_shared : int; (* number of frames for that buffer *)
    mutable buf_minor_modes : minor_mode list;
    mutable buf_major_mode : major_mode;
    mutable buf_finalizers : (unit -> unit) list;
    
    mutable buf_vars : vars;
    buf_location : location;
  } 

and major_mode = {
    maj_name : string;
    maj_map : map;
    mutable maj_hooks : (buffer -> unit) list;
    mutable maj_vars : vars;
  }

and minor_mode = {
    min_name : string;
    min_map : map;
    mutable min_hooks : (buffer -> unit) list;
    mutable min_vars : vars;
  }
  
(* a frame: a view of a buffer for a window *)
and frame  =
  {
    mutable frm_buffer : buffer;
    mutable frm_location : location;
    mutable frm_window : window;
    mutable frm_last_text_updated : int;
    mutable frm_last_buf_updated : int;
    
    mutable frm_prefix : key list;
    
    mutable frm_repeat_action : int;
    mutable frm_last_action : action;
    
    mutable frm_start : Text.point;(* first point of the first buffer-line on screen *)
    mutable frm_end : Text.point; (* last point on screen, -1 if modified *)
    mutable frm_y_offset : int; (* offset(+/-) of screen-lines after frm_start *)
    mutable frm_point : Text.point; (* insert point *)
    
    mutable frm_cursor_x : int;
    mutable frm_cursor_y : int;
    mutable frm_cursor : string;
    mutable frm_cursor_attr : Text.attribute;
    
    mutable frm_force_point : bool;
    mutable frm_force_start : bool;
    mutable frm_force_cursor : bool;
    
    mutable frm_x_offset : int;
    mutable frm_cutline : int; (* max_int for no, else length *)
    
    mutable frm_has_scrollbar : int;(* 0 for no scrollbar, 2 for scrollbar *)
    mutable frm_has_status_line : int;(* 0 for minibuffer, 1 for normal frame *)
    mutable frm_status : status;    
    
    mutable frm_xpos : int;
    mutable frm_ypos : int;
    mutable frm_width : int;
    mutable frm_height : int;
    
    mutable frm_table : line_repr array;
    mutable frm_killed : bool;
    mutable frm_mini_buffer : string option;
    mutable frm_redraw : bool;    
  } 

and status_info =
  StatModified
| StatName
| StatLine
| StatCol
| StatFile
| StatMode

and status =
  { mutable status_string : string;
    mutable status_modified : bool;
    mutable status_format : (status_info * (int * int)) list;
    mutable stat_col : int;
    mutable stat_name : string;
    mutable stat_file : string;
    mutable stat_line : int;
    mutable stat_modified : bool;
    mutable stat_modes : minor_mode list;
    mutable stat_mode : major_mode;
  }

and line_repr =
  { 
    mutable repr_line : Text.line;
    mutable repr_y : int;
    mutable repr_x : int;
    mutable repr_prev_offset : int;
    mutable repr_prev_reprs : Text.repr list;
    mutable repr_offset : int;
    mutable repr_reprs : Text.repr list;
  } 

(* an xterm: a window containing some frames *)
and top_window = 
  { mutable top_location : location;
    mutable top_display : WX_xterm.xterm_display option;
    mutable top_xterm : WX_xterm.xterm_window option;
    mutable top_term : WX_xterm.t;
    top_attrs : WX_xterm.xterm_gc option array;
    mutable top_windows : window;
    mutable top_mini_buffers : frame list;
    mutable top_width : int;
    mutable top_height : int;
    mutable top_name : string;
    mutable top_active_frame : frame;
    mutable top_second_cursor : frame option;
    mutable top_root : WX_root.t;
    mutable top_appli : WX_appli.t;
    mutable top_scrollbar : WX_adjust.t;
  } 

and window =
  { 
    mutable win_xpos : int;
    mutable win_ypos : int;
    mutable win_width : int;
    mutable win_height : int;
    mutable win_down : window_down;
    mutable win_up : window_up;
    mutable win_mini : bool;
  } 

and window_up =
  Window of window
| TopWindow of top_window

and window_down =
| HComb of window * window 
| VComb of window * window
| NoFrame of unit
| WFrame of frame

and location =
  { 
    loc_map : map;
    mutable loc_windows : top_window list;
    mutable loc_buffers : (string, buffer) Hashtbl.t;
    mutable loc_files : (string, buffer) Hashtbl.t;
    mutable loc_dirname : string;
    mutable loc_width : int;
    mutable loc_height : int;
    mutable loc_fg : string;
    mutable loc_bg : string;
    mutable loc_font : string;
    loc_vars : vars;
    mutable loc_counter : int;
(*    
    loc_vars_table : (string, (Obj.t -> string) * (string -> Obj.t)) Hashtbl.t;
  *)  
    loc_fonts : (string,int) Hashtbl.t;
    loc_fonts_names : string array;
    mutable loc_fonts_n : int;
    loc_colors : (string,int) Hashtbl.t;
    loc_colors_names : string array;
    mutable loc_colors_n : int;
    
    loc_mutex : Concur.Mutex.t;
  } 

type sens = 
  Backward | 
  Forward
type to_regexp =
  Regexp
| RegexpString

(*************************************************************************)
               (*      Values      *)
(*************************************************************************)

(* Les hooks de lancement apres le chargement d'un module *)

let start_hooks = ref []
let add_start_hook hook = start_hooks := hook :: !start_hooks

let init (location : location) =
  let rec iter hooks =
    match hooks with
      [] -> ()
    | (f : location -> unit) :: hooks -> 
        f location;
        iter hooks
  in
  let hooks = List.rev !start_hooks in
  start_hooks := [];
  iter hooks

  (* Les variables locales *)
  
let set_global location var value = Local.set location.loc_vars var value
let set_local buf var value = Local.set buf.buf_vars var value
let get_var buf var = 
  try Local.get buf.buf_vars var with Not_found ->
      try
        Local.get buf.buf_major_mode.maj_vars var with
        Not_found ->
          let rec iter list =
            match list with
              [] -> Local.get buf.buf_location.loc_vars var
            | min :: list -> 
                try
                  Local.get min.min_vars var
                with _ -> iter list
          in
          iter buf.buf_minor_modes
          
let get_global location var = Local.get location.loc_vars var
let get_local buf var = Local.get buf.buf_vars var
  
let set_minor_var min var value = Local.set min.min_vars var value
let set_major_var maj var value = Local.set maj.maj_vars var value
  
let rec exec_hooks hooks arg =
  match hooks with
    [] -> ()
  | f :: hooks -> 
      (try f arg with _ -> ());
      exec_hooks hooks arg

let add_hook location hook_var hook =
  let tail = try
      get_global location hook_var
    with _ -> [] in
  set_global location hook_var (hook :: tail)
  
(*************************************************************************)
               (*      Initialization      *)
(*************************************************************************)

  
(* Les variables importantes dans le reste du programme. *)
open Options
  
  
let load_path = define_option ["efuns_path"] 
  "<load_path> is the path where modules (.cmo and .cma) can be found
  for dynamic linking." path_option []

let path = Dyneval.load_path
  
let efuns_path = [ 
      (Filename.concat homedir ".efuns") ;
      Version.efuns_lib ; 
      Version.installdir; 
      Version.ocamllib]
  
let _ = 
  path := !!load_path @ efuns_path;
  option_hook load_path (fun _ -> path := !!load_path @ efuns_path)

let init_files = ref []
let init_frames = ref []
let displayname = ref ""
let height = ref 27
let no_init = ref false
  
(*--------------------    Ressources *)

let xdefaults = try Sys.getenv "XUSERFILESEARCHPATH" with
    Not_found -> Filename.concat Utils.homedir ".Xdefaults"

let resname = ["Efuns";"efuns"]

let x_res = Xrm.create ()
let _ =
  begin    
    try
      let efuns_res = 
        let path = try Utils.string_to_path (Sys.getenv "XFILESEARCHPATH") with _ -> 
              [] in
        let xenv = try Sys.getenv "XENVIRONMENT" with _ -> "" in
        let xroot = try Filename.concat  (Sys.getenv "X11ROOT")
            "lib/X11/app-defaults/" with _ -> "" in
        Utils.find_in_path (path@[
            xenv; xroot; "/usr/X11/lib/X11/app-defaults/"]) "Efuns"
      in
      Xrm.safe_load x_res efuns_res
    with _ -> ()
  end;
  Xrm.safe_load x_res xdefaults
  
let t = x_res
  (*
  let _ = Printf.printf "%d %d %s %s %s" !width !height !font !fg !bg; 
  print_newline () 
*)
  
(*--------------------    Arguments *)

let width_opt = ref None
let height_opt = ref None
let font_opt = ref None
let fg_opt = ref None
let bg_opt = ref None
let check = ref false
  
  let _ =
  Arg.parse [
    "-d", Arg.String(fun s -> displayname :=s),"<dpy>: Name of display";
    "--display", Arg.String(fun s -> displayname :=s),"<dpy>: Name of display";
    "-fg", Arg.String(fun s -> fg_opt :=Some s), "<color>: Foreground color";
    "-bg", Arg.String(fun s -> bg_opt :=Some s), "<color>: Background color";
    "-font", Arg.String(fun s -> font_opt :=Some s), "<font>: Font name";
    "-check", Arg.Set check, ": only for testing";
    "-width", Arg.Int (fun i -> width_opt := Some i), "<len>: Width in chars";
    "-height", Arg.Int (fun i -> height_opt := Some i), "<len>: Height in chars";
    "-q", Arg.Set no_init,": Don't load init files";
    "-I",Arg.String (fun s -> load_path =:= 
        (string_to_path s) @ !!load_path), "<path>: Load Path";
    "-c", Arg.String Dyneval.compile,"<file.ml>: compile file";
    "-frame", Arg.String (fun s -> init_frames := s:: !init_frames), "<file>: open a frame with <file>";
  ] (fun name -> init_files := name :: !init_files) 
  "A small editor entirely written in Objective Caml 
by Fabrice LE FESSANT, INRIA Rocquencourt, FRANCE
  http ://pauillac.inria.fr/efuns

Options :
  " 
  
let _ =
  Options.filename := 
  (try Utils.find_in_path (Utils.homedir :: !!load_path) ".efunsrc" with
      _ -> Filename.concat Utils.homedir ".efunsrc");
  (try Options.init () with _ -> ())

open Options
  
let width = define_option ["width"] "" int_option 80
let height = define_option ["height"] "" int_option 25
let font = define_option ["font"] "" string_option "fixed"
let foreground = define_option ["foreground"] "" string_option "white"
let background = define_option ["background"] "" string_option "black"
  
let _ =
  (match !fg_opt with None -> () | Some color -> foreground =:= color);
  (match !bg_opt with None -> () | Some color -> background =:= color);
  (match !font_opt with None -> () | Some color -> font =:= color);
  (match !width_opt with None -> () | Some color -> width =:= color);
  (match !height_opt with None -> () | Some color -> height =:= color)  
    
let (actions : (string, generic_action) Hashtbl.t) = Hashtbl.create 63

let define_action action_name action_fun =
  (try ignore (Hashtbl.find actions action_name);
      Printf.printf "Warning: action \"%s\" defined twice" action_name;
      print_newline ();
    with _ -> ());
  Hashtbl.add actions action_name (FrameAction action_fun)

let define_buffer_action action_name action_fun =
  (try ignore (Hashtbl.find actions action_name);
      Printf.printf "Warning: action \"%s\" defined twice" action_name;
      print_newline ();
    with _ -> ());
  Hashtbl.add actions action_name (BufferAction action_fun)

let no_action = BufferAction (fun _ -> ())
let get_action action =
  try Hashtbl.find actions action with Not_found -> no_action

let execute_action action frame = 
  match (get_action action) with
    BufferAction f -> f frame.frm_buffer
  | FrameAction f -> f frame 

let execute_buffer_action action buf =
  match (get_action action) with
    BufferAction f -> f buf
  | FrameAction f -> 
      Printf.printf "Can't apply action %s on buffer" action;
      print_newline ()
      
      
let string_to_regex s = s, Str.regexp s
    
let regexp_option = define_option_class "Regexp" 
    (fun v -> match v with
        Value s ->  string_to_regex s | _ -> raise Not_found)
  (fun (s,r) -> Value s)
