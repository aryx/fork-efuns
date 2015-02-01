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



open Options
open Utils
open Interactive
open Efuns
open Keymap
open Simple
open Select
open Search
open Eval
open Compil
open Abbrevs
open Complex
open System
open Multi_frames
open Top_window
  

let grep_hist = ref ["grep -n "]

let save_options frame = Options.save ()
let fondamental_mode frame =
  Ebuffer.set_major_mode frame.frm_buffer Ebuffer.fondamental_mode
  
let _ =
  define_action "fill_paragraph" fill_paragraph;
  define_action "save_options" save_options;
  define_action "fondamental_mode" fondamental_mode;
  define_action "replace_string" replace_string;
  define_action "replace_regexp" replace_regexp;
  define_action "query_replace_string" query_replace_string;
  define_action "query_replace_regexp" query_replace_regexp;
  define_action "load_library" load_library;
  define_action "get_position" get_pos;
  define_action "open_display" open_display;
(*  define_action "compile" (compile c_find_error); *)
  define_action "grep" grep;
  define_action "goto_line" goto_line;
  define_action "goto_char" goto_char;
  define_action "unset_attr" unset_attr;
  define_action "eval" Complex.eval;  
  define_action "start_server" Server.start;  
  define_action "delete_before_point"  
    delete_backspace_char;
  define_action "insert_return"  insert_return;
  define_action "forward_line"  forward_line;  
  define_action "backward-line"  backward_line;
  define_action "backward_screen"  backward_screen;
  define_action "forward_screen"  forward_screen;
  
  define_action "backward_paragraph"  (to_frame backward_paragraph);
  define_action "kill_region"  kill_region;
  define_action "forward_paragraph"  (to_frame forward_paragraph);  
  define_action "beginning_of_line"  beginning_of_line;
  define_action "delete_at_point"  delete_char;
  define_action "move_backward"  (fun frame -> ignore (move_backward frame 1));
  define_action "move_forward"  
    (fun frame -> ignore (move_forward frame 1));
  define_action "end_of_line"  end_of_line;
  define_action "kill_end_of_line"  kill_end_of_line;
  define_action "isearch_forward"  isearch_forward    ;
  define_action "isearch_backward"  isearch_backward    ;
  define_action "undo"  undo;
  define_action "forward_word"  (to_frame forward_word);  
  define_action "backward_word"  (to_frame backward_word);
  define_action "end_of_file"  end_of_file;
  define_action "begin_of_file"  begin_of_file;
  define_action "insert_killed"  insert_killed;
  define_action "mark_at_point"  mark_at_point;
  define_action "recenter"  recenter;
  define_action "transpose_chars"  (to_frame transpose_chars);
  define_action "hungry_electric_delete"  hungry_electric_delete;

  define_action "isearch_forward_regexp"  isearch_forward_regexp;
  define_action "isearch_backward_regexp"  isearch_backward_regexp;
  define_action "delete_forward_word"  (to_frame delete_forward_word);
  define_action "delete_backward_word"  (to_frame delete_backward_word);
  define_action "call_interactive"  call_interactive;
  define_action "shell_command"  shell_command;
  define_action "insert_next_killed"  insert_next_killed;
  define_action "transpose_words"  
  (to_frame transpose_words);
  define_action "lowercase_word" 
    (fun frame ->
      let buf = frame.frm_buffer in
      let point = frame.frm_point in
      on_word buf point String.lowercase);
  define_action "uppercase_word" 
    (fun frame ->
      let buf = frame.frm_buffer in
      let point = frame.frm_point in
      on_word buf point String.uppercase);
  define_action "revert_buffer" reload;
  define_action "check_file" check_file;
  define_buffer_action "update_time" update_time;
(* C-x map *)
  define_action "load_buffer"  load_buffer;
  define_action "insert_file"  insert_file;
  define_action "exit"  exit_efuns; 
  define_action "change_buffer"  change_buffer;
  define_action "change_font"  change_font;
  define_action "vertical_cut_frame"  v_cut_frame;    
  define_action "horizontal_cut_frame"  h_cut_frame;    
  define_action "one_frame"  one_frame;
  define_action "delete_frame"  delete_frame;
  define_action "next_frame"  next_frame;
  define_action "kill_buffer"  kill_buffer;
  define_action "save_buffer"  save_buffer; 
  define_action "next_error"  next_error;
  define_action "write_file"  write_buffer; 
  define_action "save_some_buffers"  save_some_buffers;
  define_action "point_at_mark"  point_at_mark;
  
(* C-x 5 map *)
  define_action "window_load_buffer"  window_load_buffer;
  define_action "window_change_buffer"  window_change_buffer;
  define_action "delete_window"  Top_window.delete_window;

(* C-h map *)
  define_action "help_bindings"  Frame.bindings_help;

    (* C-M map *)
  define_action "left_buffer"  left_buffer;
  define_action "right_buffer"  right_buffer;
  define_action "down_buffer"  down_buffer;
  define_action "up_buffer"  up_buffer;
  define_action "next_hole" next_hole;
  
  ()
  
  
let global_map = define_option ["global_map"] "" 
    (list_option binding_option) 
  []

let interactives_map = define_option ["interactives_map"] ""
    (list_option string2_option) 
  []

let c_h = (ControlMap, Char.code 'h')
let c_x = (ControlMap, Char.code 'x') 
let c_c = (ControlMap, Char.code 'c') 
let n_5 = (NormalMap, Char.code '5') 

let _ = 
  if !!global_map = [] then begin
      global_map =:= [
        [MetaMap, XK.xk_q], "fill_paragraph";
        [NormalMap, XK.xk_BackSpace], "delete_before_point"; 
        [NormalMap, XK.xk_Delete], "delete_at_point"; 
        [NormalMap, XK.xk_Return], "insert_return"; 
        [NormalMap, XK.xk_Left], "move_backward"; 
        [NormalMap, XK.xk_Right], "move_forward"; 
        [NormalMap, XK.xk_Down], "forward_line"; 
        [NormalMap, XK.xk_Up], "backward-line"; 
        [NormalMap, XK.xk_Prior], "backward_screen"; 
        [NormalMap, XK.xk_Next], "forward_screen";
        [NormalMap, Char.code ' '], "char_expand_abbrev";
        [NormalMap, XK.xk_Insert], "overwrite_mode";

        [ControlMap, XK.xk_Up], "backward_paragraph";
        [ControlMap, Char.code 'w'], "kill_region";
        [ControlMap, XK.xk_Down], "forward_paragraph";  
        [ControlMap, Char.code 'a'], "beginning_of_line";
        [ControlMap, Char.code 'd'], "delete_at_point";
        [ControlMap, Char.code 'b'], "move_backward";
        [ControlMap, Char.code 'f'], "move_forward";
        [ControlMap, Char.code 'e'], "end_of_line";
        [ControlMap, Char.code 'k'], "kill_end_of_line";
        [ControlMap, Char.code 's'], "isearch_forward";
        [ControlMap, Char.code 'r'], "isearch_backward";
        [ControlMap, Char.code '_'], "undo";
        [ControlMap, XK.xk_Right ], "forward_word";  
        [ControlMap, XK.xk_Left ], "backward_word";
        [ControlMap, XK.xk_Next], "end_of_file";
        [ControlMap, XK.xk_Prior], "begin_of_file";
        [ControlMap, Char.code 'y'], "insert_killed";
        [ControlMap, Char.code ' '], "mark_at_point";
        [ControlMap, Char.code 'l'], "recenter";
        [ControlMap, Char.code 't'], "transpose_chars";
        [ControlMap, XK.xk_BackSpace], "hungry_electric_delete";

        
        [MetaMap, Char.code 's'], "isearch_forward_regexp";
        [MetaMap, Char.code 'r'], "isearch_backward_regexp";
        [MetaMap, XK.xk_Right ], "forward_word";
        [MetaMap, XK.xk_Left ], "backward_word";
        [MetaMap, Char.code 'd' ], "delete_forward_word";
        [MetaMap, XK.xk_BackSpace ], "delete_backward_word";
        [MetaMap, Char.code 'x'], "call_interactive";
        [MetaMap, Char.code '%'], "query_replace_string";
        [MetaMap, Char.code '!'], "shell_command";
        [MetaMap, Char.code 'y'], "insert_next_killed";
        [MetaMap, Char.code 't'], "transpose_words";
        [MetaMap, Char.code 'l'], "lowercase_word";
        [MetaMap, Char.code 'u'], "uppercase_word";
        [MetaMap, Char.code '/'], "dabbrev_expand";
        
        [c_x; ControlMap, Char.code 'f'], "load_buffer";
        [c_x; NormalMap, Char.code 'i'], "insert_file";
        [c_x; ControlMap, Char.code 'c'], "exit"; 
        [c_x; NormalMap, Char.code 'b'], "change_buffer";
        [c_x; NormalMap, Char.code 'F'], "change_font";
        [c_x; NormalMap, Char.code '2'], "vertical_cut_frame";    
        [c_x; NormalMap, Char.code '3'], "horizontal_cut_frame";    
        [c_x; NormalMap, Char.code '1'], "one_frame";
        [c_x; NormalMap, Char.code '0'], "delete_frame";
        [c_x; NormalMap, Char.code 'o'], "next_frame";
        [c_x; NormalMap, Char.code 'k'], "kill_buffer";
        [c_x; ControlMap, Char.code 's'], "save_buffer"; 
        [c_x;NormalMap, Char.code '`' ], "next_error";
        [c_x;ControlMap, Char.code 'w'], "write_file"; 
        [c_x; NormalMap, Char.code 's'], "save_some_buffers";
        [c_x; ControlMap, Char.code 'x'], "point_at_mark";
        [c_x; n_5; NormalMap, Char.code 'f'], "window_load_buffer";
        [c_x; n_5; NormalMap, Char.code 'b'], "window_change_buffer";
        [c_x; n_5; NormalMap, Char.code '0'], "delete_window";
        
        [c_h; NormalMap, Char.code 'K'], "help_bindings";

        [ ControlMap, Char.code 'c'; NormalMap, Char.code '-'], "next_hole";
        
        [ControlMetaMap, XK.xk_Left], "left_buffer";
        [ControlMetaMap, XK.xk_Right], "right_buffer";
        [ControlMetaMap, XK.xk_Down], "down_buffer";
        [ControlMetaMap, XK.xk_Up], "up_buffer";
      
      ]
    end;
  if !!interactives_map = [] then begin
      interactives_map =:= [
        "fondamental_mode", "fondamental_mode";
        "save_options", "save_options";
        "replace_string", "replace_string";
        "replace_regexp", "replace_regexp";
        "query_replace_string", "query_replace_string";
        "query_replace_regexp", "query_replace_regexp";
        "load_library", "load_library";
        "get_position", "get_position";
        "open_display", "open_display";
        "compile", "compile";
        "grep", "grep";
        "goto_line", "goto_line";
        "goto_char", "goto_char";
        "unset_attr", "unset_attr";
        "eval", "eval";  
        "start_server", "start_server";
        "makefile_mode", "makefile_mode";
        "ocaml_mode", "ocaml_mode";
        "tex_mode", "tex_mode";
        "c_mode", "c_mode";
        "accents_mode", "accents_mode";
        "paren_mode", "paren_mode";
        "abbrevs_mode", "abbrevs_mode";
        "ocaml_minor_mode", "ocaml_minor_mode";
        "fill_mode", "fill_mode";
        "ocaml_compiler_mode", "ocaml_compiler_mode";
        "tab_mode", "tab_mode";
        "overwrite_mode", "overwrite_mode";
      ]
    end    
    
let init_global_map location = 
  
  List.iter (fun (keys, action) ->
      try
        Keymap.add_global_key location keys action (execute_action action)
      with e ->
          Log.printf "Error for action %s" action;
          Log.exn "%s\n" e;
  ) !!global_map;
  
  Keymap.add_global_key location [NormalMap, XK.xk_dead_circumflex]
    "circumflex" (char_insert_command '^');

        (* These keys depend on the next key pressed. In my case, I don't use
  the accents, so I prefer them to immediatly enter the good key.
let	xk_dead_grave					= 0xFE50
let	xk_dead_acute					= 0xFE51
let	xk_dead_circumflex				= 0xFE52
let	xk_dead_tilde					= 0xFE53
let	xk_dead_macron					= 0xFE54
let	xk_dead_breve					= 0xFE55
let	xk_dead_abovedot				= 0xFE56
let	xk_dead_diaeresis				= 0xFE57
let	xk_dead_abovering				= 0xFE58
let	xk_dead_doubleacute				= 0xFE59
let	xk_dead_caron					= 0xFE5A
let	xk_dead_cedilla					= 0xFE5B
let	xk_dead_ogonek					= 0xFE5C
let	xk_dead_iota					= 0xFE5D
let	xk_dead_voiced_sound				= 0xFE5E
let	xk_dead_semivoiced_sound			= 0xFE5F
let	xk_dead_belowdot				= 0xFE60
*)
  
  List.iter (fun (name, action) ->
      try
      add_interactive location.loc_map name (execute_action action)
      with e ->
          Log.printf "Error for action %s" action;
          Log.exn "%s\n" e;
  ) !!interactives_map;
    
  let gmap = location.loc_map in

(* standard keys *)

(* Mouse *)
  add_global_key location [NormalMap, XK.xk_Pointer_Button1]
    "set_active_frame" mouse_set_frame;
  add_global_key location [NormalMap, XK.xk_Pointer_Button2]
    "insert_at_point" mouse_yank_at_click;
  add_global_key location [NormalMap, XK.xk_Pointer_Button3]
  "mouse_save_then_kill" mouse_save_then_kill;
  ()
  
open WX_filesel

let select_open_file frame =
  let window = frame.frm_window in
  let top_window = Window.top window in
  let cdir = Frame.current_dir frame in
  let info = {
      filter = Filename.concat cdir "*";
      current_selection = cdir;
      predicat = (fun _ -> true);
      action = (fun _ -> ());
      cancel = (fun _ -> ());
    } in
  let query = new WX_filesel.t top_window.top_root info [] in
  query#setWM_TRANSIENT_FOR (top_window.top_appli#top :> WX_types.container);
  info.action <- (fun name ->
      wrap top_window (fun top_window ->
          query#destroy;
          let _ = Frame.load_file window name in ()
      ) ());
  info.cancel <- (fun () ->
      wrap top_window (fun top_window ->
          query#destroy;
      ) ());        
  query#show

let compile frame =
  exec_interactive (buf_interactives frame.frm_buffer) frame "compile"

let _ =
  define_action "select_open_file" select_open_file;
  define_action "compile" compile;
  define_action "v_cut_frame" v_cut_frame;
  ()
  
let _ =
  if !!file_menu = [] then begin
      file_menu =:=    [
        "Open File", "select_open_file";
        "Save Buffer", "save_buffer";
        "Kill Buffer", "kill_buffer";
        "Compile", "compile";
        "", "";
        "Quit", "exit";
      ]
    end;
  if !!edit_menu = [] then begin
      edit_menu =:= [ 
        "Cut", "kill_region";
        "Paste",  "insert_killed";
        "Undo", "undo";
        "", "";
        "Cut Frame", "v_cut_frame";
        "One Frame", "one_frame";
        "Delete Frame", "delete_frame";
      ];
    end;
  help_menu := [|
    "Key Bindings", (fun frame ->
        Frame.change_buffer frame.frm_window "*bindings*"
    );
    "Changes", (fun frame ->
        let _ = Frame.load_file frame.frm_window (
            Version.efuns_lib ^"/Changes") in ());
    "About Efuns", (fun frame ->
(*
        let top_window = Window.top frame.frm_window in        
        let dialog = new WX_dialog.t top_window.top_root 
            "Efuns,\nVersion 015\nFabrice Le Fessant\nFabrice.Le_Fessant@inria.fr" [] in
        dialog#add_button "OK" (fun _ -> dialog#destroy);
        dialog#show;
    *)
        Frame.change_buffer frame.frm_window "*help*"
    );
  |];
  buffers_menu := (fun top_window button () ->
      let buffers = ref [] in
      let location = top_window.top_location in
      Hashtbl.iter (fun name buf -> buffers:=name :: !buffers) 
      location.loc_buffers;
      let desc = Array.map (fun name -> 
            (name, wrap top_window (fun top_window ->
                  let frame = top_window.top_active_frame in
                  let window = frame.frm_window in
                  Frame.change_buffer window name
              )))
        (Array.of_list !buffers) in
      let menu = new WX_popup.t top_window.top_root desc in
      let (x,y) = button#root_coordinates in
      menu#popup_once x (y + button#height) (Some !WX_types.button_event)
      )
  
let _ =
  Efuns.add_start_hook (fun location ->
      add_option_parameter location compile_find_makefile;
      add_option_parameter location Text.add_amount;
      init_global_map location)
