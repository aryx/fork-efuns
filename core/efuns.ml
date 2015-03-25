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

(*************************************************************************)
               (*      Types      *)
(*************************************************************************)
open Utils
open Local

(*s: function Efuns.error *)
let error f x =
  print_string ("error: ");
  Printf.printf f x;
  print_newline ()
(*e: function Efuns.error *)
  
(*s: exception Efuns.UnboundKey *)
exception UnboundKey
(*e: exception Efuns.UnboundKey *)

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

(*s: type Efuns.buffer *)
(* a buffer containing a file in Text.t *)
and buffer =
  { 
    mutable buf_text : Text.t;

    mutable buf_name : string;
    mutable buf_filename : string option;

    (*s: [[Efuns.buffer]] current position fields *)
    mutable buf_point : Text.point;
    mutable buf_start : Text.point;
    (*e: [[Efuns.buffer]] current position fields *)
    (*s: [[Efuns.buffer]] history fields *)
    mutable buf_modified : int; (* version? *)
    (*x: [[Efuns.buffer]] history fields *)
    mutable buf_last_saved : int;
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
    (*x: [[Efuns.buffer]] other fields *)
    mutable buf_charreprs : string array; (* 256 array *)
    (*e: [[Efuns.buffer]] other fields *)
  } 
(*e: type Efuns.buffer *)

(*s: type Efuns.major_mode *)
and major_mode = {
    maj_name : string;
    maj_map : map;

    mutable maj_hooks : (buffer -> unit) list;
    mutable maj_vars : vars;
  }
(*e: type Efuns.major_mode *)

(*s: type Efuns.minor_mode *)
and minor_mode = {
    min_name : string;
    min_map : map;

    mutable min_hooks : (buffer -> unit) list;
    mutable min_vars : vars;
  }
(*e: type Efuns.minor_mode *)
  
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
    mutable frm_table : frm_line array;
    (*x: [[Efuns.frame]] other fields *)
    (* ?? *)
    mutable frm_x_offset : int;
    (* offset(+/-) of screen-lines after frm_start *)
    mutable frm_y_offset : int;
    (*x: [[Efuns.frame]] other fields *)
    (* ?? *)
    mutable frm_cutline : int; (* max_int for no, else length *)
    (*x: [[Efuns.frame]] other fields *)
    mutable frm_force_start : bool;
    (*x: [[Efuns.frame]] other fields *)
    mutable frm_last_action : action;
    (*x: [[Efuns.frame]] other fields *)
    mutable frm_killed : bool;
    (*x: [[Efuns.frame]] other fields *)
    mutable frm_cursor_x : int;
    mutable frm_cursor_y : int;
    mutable frm_cursor : string;
    mutable frm_cursor_attr : Text.attribute;
    (*x: [[Efuns.frame]] other fields *)
    mutable frm_prefix : key list;
    (*x: [[Efuns.frame]] other fields *)
    mutable frm_repeat_action : int;
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

    (*s: [[Efuns.frm_line]] other fields *)
    mutable repr_y : int;
    mutable repr_x : int;


    mutable repr_offset : int;
    mutable frmline_boxes : Text.box list;
    (*x: [[Efuns.frm_line]] other fields *)
    (* previous values, so can check if the line changed *)
    mutable repr_prev_offset : int;
    mutable prev_frmline_boxes : Text.box list;
    (*e: [[Efuns.frm_line]] other fields *)
  } 
(*e: type Efuns.line_repr *)

(*s: type Efuns.top_window *)
(* an xterm: a window containing some frames *)
and top_window = 
  { 
    mutable top_name : string;

    mutable top_width : int;
    mutable top_height : int; (* including minibuffer line *)

    mutable window : window;

    (*s: [[Efuns.top_window]] other fields *)
    mutable graphics : Xdraw.graphics_backend option;
    (*x: [[Efuns.top_window]] other fields *)
    mutable top_active_frame : frame;
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

(*s: type Efuns.location *)
type location =
  { 
    (* key is buffer name *)
    mutable loc_buffers : (string, buffer) Hashtbl.t;
    (* key is filename *)
    mutable loc_files : (string, buffer) Hashtbl.t;

    (* list??*)
    mutable top_windows : top_window list;

    (* pwd of efuns when started *)
    mutable loc_dirname : string;

    (* general look, configurable via -xxx command line options or .efunsrc *)
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

(*************************************************************************)
               (*      Values      *)
(*************************************************************************)

(*s: global Efuns.global_location *)
let global_location = ref None
(*e: global Efuns.global_location *)
(*s: function Efuns.location *)
let location () =
  match !global_location with
  | None -> failwith "no global location defined"
  | Some x -> x
(*e: function Efuns.location *)

(*s: constant Efuns.start_hooks *)
(* Les hooks de lancement apres le chargement d'un module *)
let start_hooks = (ref []: (unit -> unit) list ref)
(*e: constant Efuns.start_hooks *)
(*s: function Efuns.add_start_hook *)
let add_start_hook hook = 
  start_hooks := hook :: !start_hooks
(*e: function Efuns.add_start_hook *)


  (* Les variables locales *)
  
(*s: function Efuns.set_global *)
let set_global var value = 
  Local.set (location()).loc_vars var value
(*e: function Efuns.set_global *)
(*s: function Efuns.set_local *)
let set_local buf var value = 
  Local.set buf.buf_vars var value
(*e: function Efuns.set_local *)
(*s: function Efuns.get_var *)
let get_var buf var = 
  try 
    Local.get buf.buf_vars var 
  with Not_found ->
    try 
      (*s: [[Efuns.get_var()]] try with major mode variables *)
      Local.get buf.buf_major_mode.maj_vars var
      (*e: [[Efuns.get_var()]] try with major mode variables *)
    with Not_found ->
      try 
        (*s: [[Efuns.get_var()]] try with minor mode variables *)
        let rec iter list =
          match list with
          | [] -> raise Not_found
          | min :: list -> 
              try
                Local.get min.min_vars var
              with _ -> iter list
        in
        iter buf.buf_minor_modes
        (*e: [[Efuns.get_var()]] try with minor mode variables *)
      with Not_found ->
        Local.get (location()).loc_vars var
(*e: function Efuns.get_var *)
          
(*s: function Efuns.get_global *)
let get_global var = 
  Local.get (location()).loc_vars var
(*e: function Efuns.get_global *)
(*s: function Efuns.get_local *)
let get_local buf var = 
  Local.get buf.buf_vars var
(*e: function Efuns.get_local *)
  
(*s: function Efuns.set_minor_var *)
let set_minor_var min var value = 
  Local.set min.min_vars var value
(*e: function Efuns.set_minor_var *)
(*s: function Efuns.set_major_var *)
let set_major_var maj var value = 
  Local.set maj.maj_vars var value
(*e: function Efuns.set_major_var *)
  
(*s: function Efuns.exec_hooks *)
let exec_hooks hooks arg =
  hooks |> List.iter (fun f ->
    try f arg 
    with exn -> error "exn in hook: %s" (Common.exn_to_s exn)
  )
(*e: function Efuns.exec_hooks *)

(*s: function Efuns.add_hook *)
let add_hook hook_var hook =
  let tail = try get_global hook_var with _ -> [] in
  set_global hook_var (hook :: tail)
(*e: function Efuns.add_hook *)

let with_lock f =
  let loc = location () in
  Mutex.lock loc.loc_mutex;
  Common.finalize f (fun () -> Mutex.unlock loc.loc_mutex)
  
(*************************************************************************)
               (*      Initialization      *)
(*************************************************************************)
  
(* Les variables importantes dans le reste du programme. *)
open Options

(*s: constant Efuns.check *)
let check = ref false
(*e: constant Efuns.check *)

(*s: constants Efuns.debug_xxx *)
let debug = ref false
let debug_graphics = ref false
let debug_display = ref false
let debug_init = ref false
(*e: constants Efuns.debug_xxx *)

(*s: constant Efuns.load_path *)
let load_path = define_option ["efuns_path"] 
  "<load_path> is the path where modules (.cmo and .cma) can be found
  for dynamic linking." path_option []
(*e: constant Efuns.load_path *)

(*s: constant Efuns.path *)
let path = (*Dyneval.load_path*) ref []
(*e: constant Efuns.path *)
  
(*s: constant Efuns.efuns_path *)
let efuns_path = [ 
      (Filename.concat homedir ".efuns") ;
(*
      Version.efuns_lib; 
      Version.installdir; 
      Version.ocamllib
*)
  ]
(*e: constant Efuns.efuns_path *)
  
(*s: toplevel Efuns._1 *)
let _ = 
  path := !!load_path @ efuns_path;
  option_hook load_path (fun _ -> path := !!load_path @ efuns_path)
(*e: toplevel Efuns._1 *)

(* used in some major mode *)
(*s: constant Efuns.font *)
let font = define_option ["font"] "" string_option "Menlo 18"
(*e: constant Efuns.font *)

  
(*--------------------    Ressources *)
(*s: constant Efuns.xdefaults *)
let xdefaults = try Sys.getenv "XUSERFILESEARCHPATH" with
    Not_found -> Filename.concat Utils.homedir ".Xdefaults"
(*e: constant Efuns.xdefaults *)

(*s: constant Efuns.x_res *)
(*let x_res = Xrm.create ()*)
(*e: constant Efuns.x_res *)
(*s: toplevel Efuns._2 *)
(*
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
*)
(*e: toplevel Efuns._2 *)
  
(*s: constant Efuns.t *)
(*let t = x_res*)
(*e: constant Efuns.t *)
  (*
  let _ = Printf.printf "%d %d %s %s %s" !width !height !font !fg !bg; 
  print_newline () 
*)
  

(*s: global Efuns.actions *)
let (actions : (action_name, generic_action) Hashtbl.t) = 
  Hashtbl.create 63
(*e: global Efuns.actions *)

(*s: function Efuns.define_action *)
let define_action action_name action_fun =
  (*s: sanity check action defined twice *)
  (try 
      Hashtbl.find actions action_name |> ignore;
      error "action \"%s\" defined twice" action_name;
   with _ -> ()
  );
  (*e: sanity check action defined twice *)
  Hashtbl.add actions action_name (FrameAction action_fun)
(*e: function Efuns.define_action *)

(*s: function Efuns.define_buffer_action *)
let define_buffer_action action_name action_fun =
  (*s: sanity check action defined twice *)
  (try 
      Hashtbl.find actions action_name |> ignore;
      error "action \"%s\" defined twice" action_name;
   with _ -> ()
  );
  (*e: sanity check action defined twice *)
  Hashtbl.add actions action_name (BufferAction action_fun)
(*e: function Efuns.define_buffer_action *)

(*s: function Efuns.get_action *)
let get_action action =
  try Hashtbl.find actions action 
  with Not_found ->
    error "Could not find action %s. Forgot define_action()?" action;
    BufferAction (fun _ -> ())
(*e: function Efuns.get_action *)

(*s: function Efuns.execute_action *)
let execute_action action = 
  match (get_action action) with
  | FrameAction f -> f 
  | BufferAction f -> (fun frame -> f frame.frm_buffer)
(*e: function Efuns.execute_action *)

(*s: function Efuns.execute_buffer_action *)
let execute_buffer_action action buf =
  match (get_action action) with
    BufferAction f -> f buf
  | FrameAction _f -> 
      error "Can't apply action %s on buffer" action
(*e: function Efuns.execute_buffer_action *)
      
(*s: function Efuns.string_to_regex *)
let string_to_regex s = s, Str.regexp s
(*e: function Efuns.string_to_regex *)
    
(*s: constant Efuns.regexp_option *)
let regexp_option = define_option_class "Regexp" 
    (fun v -> match v with
        Value s ->  string_to_regex s | _ -> raise Not_found)
  (fun (s,_r) -> Value s)
(*e: constant Efuns.regexp_option *)
(*e: core/efuns.ml *)
