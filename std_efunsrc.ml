(*s: std_efunsrc.ml *)
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
open Utils
open Options

open Interactive
open Efuns
open Keymap
open Simple
open Select
open Search
open Compil
open Abbrevs
open Complexe
open System
open Multi_frames
open Top_window
(*open Eval*)
  

(*s: constant Std_efunsrc.grep_hist *)
let grep_hist = ref ["grep -n "]
(*e: constant Std_efunsrc.grep_hist *)

(*s: function Std_efunsrc.save_options *)
let save_options frame = 
  Options.save ()
(*e: function Std_efunsrc.save_options *)
(*s: function Std_efunsrc.fondamental_mode *)
let fondamental_mode frame =
  Ebuffer.set_major_mode frame.frm_buffer Ebuffer.fondamental_mode
(*e: function Std_efunsrc.fondamental_mode *)

(*s: function Std_efunsrc.compile *)
let compile frame =
  exec_interactive (buf_interactives frame.frm_buffer) frame "compile"
(*e: function Std_efunsrc.compile *)

(*s: toplevel Std_efunsrc._1 *)
let _ =
  (*s: actions definitions *)
  (* ----------------------------------------------------------- *)
  (* Loading *)
  (* ----------------------------------------------------------- *)
  (*s: loading actions *)
  (* C-x map *)
  define_action "load_buffer"  load_buffer;
  (*x: loading actions *)
  (* C-x map *)
  define_action "insert_file"  insert_file;
  (*e: loading actions *)

  (* ----------------------------------------------------------- *)
  (* Navigating (in the file) *)
  (* ----------------------------------------------------------- *)
  (*s: navigating actions *)
  define_action "move_backward"  (fun frame -> ignore (move_backward frame 1));
  define_action "move_forward"   (fun frame -> ignore (move_forward frame 1));

  define_action "backward_word"  (to_frame backward_word);
  define_action "forward_word"  (to_frame forward_word);  

  define_action "beginning_of_line"  beginning_of_line;
  define_action "end_of_line"  end_of_line;

  define_action "backward_line"  backward_line;
  define_action "forward_line"  forward_line;  

  define_action "backward_paragraph"  (to_frame backward_paragraph);
  define_action "forward_paragraph"  (to_frame forward_paragraph);  

  define_action "backward_screen"  backward_screen;
  define_action "forward_screen"  forward_screen;

  define_action "begin_of_file"  begin_of_file;
  define_action "end_of_file"  end_of_file;

  (* M-x *)
  define_action "goto_char" goto_char;
  define_action "goto_line" goto_line;
  (*e: navigating actions *)

  (* ----------------------------------------------------------- *)
  (* Editing *)
  (* ----------------------------------------------------------- *)
  (* ------------------------- *)
  (* Inserting *)
  (* ------------------------- *)
  (*s: inserting actions *)
  define_action "insert_return"  insert_return;
  (*e: inserting actions *)
  (* ------------------------- *)
  (* Deleting *)
  (* ------------------------- *)
  (*s: deleting actions *)
  define_action "delete_before_point"  delete_backspace_char;
  define_action "delete_at_point"  delete_char;

  define_action "delete_forward_word"  (to_frame delete_forward_word);
  define_action "delete_backward_word"  (to_frame delete_backward_word);

  define_action "hungry_electric_delete"  hungry_electric_delete;
  define_action "kill_end_of_line"  kill_end_of_line;
  (*e: deleting actions *)
  (* ------------------------- *)
  (* Moving (Cut, copy, paste) *)
  (* ------------------------- *)
  (*s: moving actions *)
  define_action "mark_at_point"  mark_at_point;
  define_action "kill_region"  kill_region;
  define_action "insert_killed"  insert_killed;
  define_action "insert_next_killed"  insert_next_killed;
  (*e: moving actions *)
  (* ------------------------- *)
  (* Transforming *)
  (* ------------------------- *)
  (*s: transforming actions *)
  define_action "transpose_chars"  (to_frame transpose_chars);

  define_action "transpose_words"  (to_frame transpose_words);
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

  define_action "fill_paragraph" fill_paragraph;
  (*e: transforming actions *)
  (* ------------------------- *)
  (* Replacing *)
  (* ------------------------- *)
  (*s: replacing actions *)
  define_action "replace_string" replace_string;
  define_action "replace_regexp" replace_regexp;
  define_action "query_replace_string" query_replace_string;
  define_action "query_replace_regexp" query_replace_regexp;
  (*e: replacing actions *)

  (* ----------------------------------------------------------- *)
  (* Searching *)
  (* ----------------------------------------------------------- *)
  (*s: searching actions *)
  define_action "isearch_forward"  isearch_forward;
  define_action "isearch_backward"  isearch_backward;
  define_action "isearch_forward_regexp"  isearch_forward_regexp;
  define_action "isearch_backward_regexp"  isearch_backward_regexp;
  (*e: searching actions *)

  (* ----------------------------------------------------------- *)
  (* Undoing *)
  (* ----------------------------------------------------------- *)
  (*s: undoing actions *)
  define_action "undo"  undo;
  (*e: undoing actions *)

  (* ----------------------------------------------------------- *)
  (* External commands *)
  (* ----------------------------------------------------------- *)
  (*s: external command actions *)
  define_action "shell_command"  shell_command;
  (*x: external command actions *)
  define_action "grep" grep;
  (*  define_action "compile" (compile c_find_error); *)
  (*e: external command actions *)

  (* ----------------------------------------------------------- *)
  (* Buffers/windows/frames *)
  (* ----------------------------------------------------------- *)
  (*s: buffer managment actions *)
  (* C-x map *)
  define_action "change_buffer"  change_buffer;
  (*x: buffer managment actions *)
  (* C-x map *)
  define_action "kill_buffer"  kill_buffer;
  (*e: buffer managment actions *)
  (*s: buffer navigating actions *)
  (* C-M map *)
  define_action "left_buffer"  left_buffer;
  (* C-M map *)
  define_action "right_buffer"  right_buffer;
  (* C-M map *)
  define_action "down_buffer"  down_buffer;
  (* C-M map *)
  define_action "up_buffer"  up_buffer;
  (*e: buffer navigating actions *)
  (*s: frame managment actions *)
  (* C-x map *)
  define_action "vertical_cut_frame"  v_cut_frame;    
  (* C-x map *)
  define_action "horizontal_cut_frame"  h_cut_frame;    
  (*x: frame managment actions *)
  (* C-x map *)
  define_action "one_frame"  one_frame;
  (* C-x map *)
  define_action "delete_frame"  delete_frame;
  (*e: frame managment actions *)
  (*s: frame navigation actions *)
  (* C-x map *)
  define_action "next_frame"  next_frame;
  (*e: frame navigation actions *)

  (* ----------------------------------------------------------- *)
  (* Meta *)
  (* ----------------------------------------------------------- *)
  (*s: meta actions *)
  define_action "call_interactive"  call_interactive;
  (*x: meta actions *)
  define_action "eval" Complexe.eval;  
  (*e: meta actions *)

  (* ----------------------------------------------------------- *)
  (* Saving *)
  (* ----------------------------------------------------------- *)
  (*s: saving actions *)
  (* C-x map *)
  define_action "save_buffer"  save_buffer; 
  (*x: saving actions *)
  (* C-x map *)
  define_action "save_some_buffers"  save_some_buffers;
  (* C-x map *)
  define_action "write_file"  write_buffer; 
  (*e: saving actions *)

  (* ----------------------------------------------------------- *)
  (* Major mode *)
  (* ----------------------------------------------------------- *)
  (*s: major mode actions *)
  define_action "fondamental_mode" fondamental_mode;
  (*e: major mode actions *)

  (* ----------------------------------------------------------- *)
  (* Misc *)
  (* ----------------------------------------------------------- *)
  (*s: misc actions *)
  define_action "recenter"  recenter;
  define_action "revert_buffer" reload;

  define_action "open_display" open_display;
  (* C-x map *)
  define_action "change_font"  change_font;
  (*x: misc actions *)
  define_action "check_file" check_file;
  (*x: misc actions *)
  define_action "save_options" save_options;
  (*x: misc actions *)
  define_action "get_position" get_pos;
  (*x: misc actions *)
  define_action "unset_attr" unset_attr;
  (*x: misc actions *)
  define_buffer_action "update_time" update_time;
  (*x: misc actions *)
  (* C-x map *)
  define_action "exit"  exit_efuns; 
  (*x: misc actions *)
  (* C-h map *)
  define_action "help_bindings"  Frame.bindings_help;
  (*x: misc actions *)
  (* C-x map *)
  define_action "point_at_mark"  point_at_mark;
  (*x: misc actions *)
  (* C-x map *)
  define_action "next_error"  next_error;
  (*x: misc actions *)
  define_action "compile" compile;
  (*x: misc actions *)
  (*s: window managment actions *)
  (* C-x 5 map *)
  define_action "window_load_buffer"  window_load_buffer;
  (* C-x 5 map *)
  define_action "window_change_buffer"  window_change_buffer;
  (* C-x 5 map *)
  define_action "delete_window"  Top_window.delete_window;
  (*e: window managment actions *)
  (*x: misc actions *)
  (* C-M map *)
  define_action "next_hole" next_hole;
  (*e: misc actions *)
  (*e: actions definitions *)
  ()
(*e: toplevel Std_efunsrc._1 *)
  
  
(*s: constant Std_efunsrc.global_map *)
let global_map = define_option ["global_map"] "" (list_option binding_option) []
(*e: constant Std_efunsrc.global_map *)
(*s: constant Std_efunsrc.interactives_map *)
let interactives_map = define_option ["interactives_map"] ""
    (list_option string2_option) 
  []
(*e: constant Std_efunsrc.interactives_map *)

(*s: constant Std_efunsrc.c_h *)
let c_h = (ControlMap, Char.code 'h')
(*e: constant Std_efunsrc.c_h *)
(*s: constant Std_efunsrc.c_x *)
let c_x = (ControlMap, Char.code 'x') 
(*e: constant Std_efunsrc.c_x *)
(*s: constant Std_efunsrc.c_c *)
let c_c = (ControlMap, Char.code 'c') 
(*e: constant Std_efunsrc.c_c *)
(*s: constant Std_efunsrc.n_5 *)
let n_5 = (NormalMap, Char.code '5') 
(*e: constant Std_efunsrc.n_5 *)

(*s: toplevel Std_efunsrc._2 *)
let _ = 
  if !!global_map = [] then begin
      global_map =:= [
        (*s: [[global_map]] initial entries *)
        (* -------------------------------------------------------- *)
        (* Loading *)
        (* -------------------------------------------------------- *)
        (*s: loading keys *)
        [c_x; ControlMap, Char.code 'f'], "load_buffer";
        (*x: loading keys *)
        [c_x; NormalMap, Char.code 'i'], "insert_file";
        (*e: loading keys *)

        (* -------------------------------------------------------- *)
        (* Navigating (in the file) *)
        (* -------------------------------------------------------- *)
        (*s: navigating keys *)
        [NormalMap, XK.xk_Left], "move_backward"; 
        [NormalMap, XK.xk_Right], "move_forward"; 
        [ControlMap, Char.code 'b'], "move_backward";
        [ControlMap, Char.code 'f'], "move_forward";

        [ControlMap, XK.xk_Left ], "backward_word";
        [ControlMap, XK.xk_Right ], "forward_word";  
        [MetaMap, XK.xk_Left ], "backward_word";
        [MetaMap, XK.xk_Right ], "forward_word";

        [ControlMap, Char.code 'a'], "beginning_of_line";
        [ControlMap, Char.code 'e'], "end_of_line";

        [NormalMap, XK.xk_Up], "backward_line"; 
        [NormalMap, XK.xk_Down], "forward_line"; 

        [ControlMap, XK.xk_Up], "backward_paragraph";
        [ControlMap, XK.xk_Down], "forward_paragraph";  

        [NormalMap, XK.xk_Prior], "backward_screen"; 
        [NormalMap, XK.xk_Next], "forward_screen";

        [ControlMap, XK.xk_Next], "end_of_file";
        [ControlMap, XK.xk_Prior], "begin_of_file";
        (*x: navigating keys *)
        (* pad: *)
        [MetaMap, Char.code '>'], "end_of_file";
        [MetaMap, Char.code '<'], "begin_of_file";
        (*e: navigating keys *)

        (* -------------------------------------------------------- *)
        (* Editing *)
        (* -------------------------------------------------------- *)
        (* ------------- *)
        (* Inserting *)
        (* ------------- *)
        (*s: inserting keys *)
        [NormalMap, XK.xk_Return], "insert_return"; 
        (*e: inserting keys *)
        (* ------------- *)
        (* Deleting *)
        (* ------------- *)
        (*s: deleting keys *)
        [NormalMap, XK.xk_BackSpace], "delete_before_point"; 
        [ControlMap, Char.code 'd'], "delete_at_point";
        [NormalMap, XK.xk_Delete], "delete_at_point"; 

        [MetaMap, Char.code 'd' ], "delete_forward_word";
        [MetaMap, XK.xk_BackSpace ], "delete_backward_word";

        [ControlMap, XK.xk_BackSpace], "hungry_electric_delete";
        [ControlMap, Char.code 'k'], "kill_end_of_line";
        (*e: deleting keys *)
        (* ------------------------------ *)
        (* Moving (Cut, copy, paste) *)
        (* ------------------------------ *)
        (*s: moving keys *)
        [ControlMap, Char.code ' '], "mark_at_point";
        [ControlMap, Char.code 'w'], "kill_region";
        [ControlMap, Char.code 'y'], "insert_killed";
        [MetaMap, Char.code 'y'], "insert_next_killed";
        (*e: moving keys *)
        (* ---------------------- *)
        (* Transforming *)
        (* ---------------------- *)
        (*s: transforming keys *)
        [ControlMap, Char.code 't'], "transpose_chars";

        [MetaMap, Char.code 't'], "transpose_words";
        [MetaMap, Char.code 'l'], "lowercase_word";
        [MetaMap, Char.code 'u'], "uppercase_word";

        [MetaMap, XK.xk_q], "fill_paragraph";
        (*e: transforming keys *)
        (* ---------------------- *)
        (* Replacing *)
        (* ---------------------- *)
        (*s: replacing keys *)
        [MetaMap, Char.code '%'], "query_replace_string";
        (*e: replacing keys *)

        (* -------------------------------------------------------- *)
        (* Searching *)
        (* -------------------------------------------------------- *)
        (*s: searching keys *)
        [ControlMap, Char.code 's'], "isearch_forward";
        [ControlMap, Char.code 'r'], "isearch_backward";
        [MetaMap, Char.code 's'], "isearch_forward_regexp";
        [MetaMap, Char.code 'r'], "isearch_backward_regexp";
        (*e: searching keys *)

        (* -------------------------------------------------------- *)
        (* Undoing *)
        (* -------------------------------------------------------- *)
        (*s: undoing keys *)
        [ControlMap, Char.code '_'], "undo";
        (*x: undoing keys *)
        (* pad: *)
        [ControlMap, Char.code '/'], "undo";
        (*e: undoing keys *)

        (* -------------------------------------------------------- *)
        (* External commands *)
        (* -------------------------------------------------------- *)
        (*s: external commands keys *)
        [MetaMap, Char.code '!'], "shell_command";
        (*e: external commands keys *)

        (* -------------------------------------------------------- *)
        (* Buffers/windows/frames *)
        (* -------------------------------------------------------- *)
        (*s: buffer managment keys *)
        [c_x; NormalMap, Char.code 'b'], "change_buffer";
        (*x: buffer managment keys *)
        [c_x; NormalMap, Char.code 'k'], "kill_buffer";
        (*e: buffer managment keys *)
        (*s: buffer navigating keys *)
        [ControlMetaMap, XK.xk_Left], "left_buffer";
        [ControlMetaMap, XK.xk_Right], "right_buffer";
        [ControlMetaMap, XK.xk_Down], "down_buffer";
        [ControlMetaMap, XK.xk_Up], "up_buffer";
        (*e: buffer navigating keys *)
        (*s: frame managment keys *)
        [c_x; NormalMap, Char.code '2'], "vertical_cut_frame";    
        [c_x; NormalMap, Char.code '3'], "horizontal_cut_frame";    
        (*x: frame managment keys *)
        [c_x; NormalMap, Char.code '1'], "one_frame";
        [c_x; NormalMap, Char.code '0'], "delete_frame";
        (*e: frame managment keys *)
        (*s: frame navigation keys *)
        [c_x; NormalMap, Char.code 'o'], "next_frame";
        (*e: frame navigation keys *)

        (* -------------------------------------------------------- *)
        (* Meta *)
        (* -------------------------------------------------------- *)
        (*s: meta keys *)
        [MetaMap, Char.code 'x'], "call_interactive";
        (*e: meta keys *)

        (* -------------------------------------------------------- *)
        (* Saving *)
        (* -------------------------------------------------------- *)
        (*s: saving keys *)
        [c_x; ControlMap, Char.code 's'], "save_buffer"; 
        (*x: saving keys *)
        [c_x; NormalMap, Char.code 's'], "save_some_buffers";
        [c_x;ControlMap, Char.code 'w'], "write_file"; 
        (*e: saving keys *)

        (* -------------------------------------------------------- *)
        (* Misc *)
        (* -------------------------------------------------------- *)
        (*s: misc keys *)
        [NormalMap, XK.xk_Insert], "overwrite_mode";
        [ControlMap, Char.code 'l'], "recenter";

        [c_x; NormalMap, Char.code 'F'], "change_font";
        (*x: misc keys *)
        [c_h; NormalMap, Char.code 'v'], "get_variable";
        (*x: misc keys *)
        [c_x; ControlMap, Char.code 'c'], "exit"; 
        (*x: misc keys *)
        [c_h; NormalMap, Char.code 'K'], "help_bindings";
        (*x: misc keys *)
        [c_x; ControlMap, Char.code 'x'], "point_at_mark";
        (*x: misc keys *)
        [c_x;NormalMap, Char.code '`' ], "next_error";
        (*x: misc keys *)
        [NormalMap, Char.code ' '], "char_expand_abbrev";
        [MetaMap, Char.code '/'], "dabbrev_expand";
        (*x: misc keys *)
        (*s: window managment keys *)
        [c_x; n_5; NormalMap, Char.code 'f'], "window_load_buffer";
        [c_x; n_5; NormalMap, Char.code 'b'], "window_change_buffer";
        [c_x; n_5; NormalMap, Char.code '0'], "delete_window";
        (*e: window managment keys *)
        (*x: misc keys *)
        [ ControlMap, Char.code 'c'; NormalMap, Char.code '-'], "next_hole";
        (*e: misc keys *)
        (*e: [[global_map]] initial entries *)
      ]
    end;
  (*s: [[Std_efunsrc.toplevel]] set interactives_map *)
  if !!interactives_map = [] then begin
      interactives_map =:= List.map (fun x -> x, x ) [
        (*s: [[interactives_map]] initial entries *)
        "save_options";
        "load_library";
        "get_position";
        "open_display";
        "unset_attr";
        "start_server";
        (*x: [[interactives_map]] initial entries *)
        "goto_char";
        "goto_line";
        (*x: [[interactives_map]] initial entries *)
        "replace_string";
        "replace_regexp";
        "query_replace_string";
        "query_replace_regexp";
        (*x: [[interactives_map]] initial entries *)
        "fondamental_mode";
        (*x: [[interactives_map]] initial entries *)
        "compile";
        "grep";
        (*x: [[interactives_map]] initial entries *)
        "eval";  
        (*x: [[interactives_map]] initial entries *)
        "makefile_mode";
        "ocaml_mode";
        "tex_mode";
        "c_mode";
        (*x: [[interactives_map]] initial entries *)
        "paren_mode";
        "abbrevs_mode";
        "accents_mode";
        "ocaml_minor_mode";
        "fill_mode";
        "ocaml_compiler_mode";
        "tab_mode";
        "overwrite_mode";
        (*e: [[interactives_map]] initial entries *)
      ]
    end    
  (*e: [[Std_efunsrc.toplevel]] set interactives_map *)
(*e: toplevel Std_efunsrc._2 *)
    
(*s: function Std_efunsrc.init_global_map *)
let init_global_map location = 
  !!global_map |> List.iter (fun (keys, action) ->
      try
        Keymap.add_global_key location keys action (execute_action action)
      with e ->
        Log.printf "Error for action %s" action;
        Log.exn "%s\n" e;
  );

  (*s: [[Std_efunsrc.init_global_map()]] add interactives from interactives_map *)
  List.iter (fun (name, action) ->
      try
        add_interactive location.loc_map name (execute_action action)
      with e ->
          Log.printf "Error for action %s" action;
          Log.exn "%s\n" e;
  ) !!interactives_map;
  (*e: [[Std_efunsrc.init_global_map()]] add interactives from interactives_map *)
    
  (* standard keys *)

  (* Mouse *)
  (*s: [[Std_efunsrc.init_global_map()]] mouse keys setup *)
  add_global_key location [NormalMap, XK.xk_Pointer_Button1]
    "set_active_frame" mouse_set_frame;
  add_global_key location [NormalMap, XK.xk_Pointer_Button2]
    "insert_at_point" mouse_yank_at_click;
  add_global_key location [NormalMap, XK.xk_Pointer_Button3]
    "mouse_save_then_kill" mouse_save_then_kill;
  (*e: [[Std_efunsrc.init_global_map()]] mouse keys setup *)
  ()
(*e: function Std_efunsrc.init_global_map *)
  
(*open WX_filesel*)
 
(*s: toplevel Std_efunsrc._4 *)
let _ =
  (*s: [[Std_efunsrc]] file menu setup *)
  if !!file_menu = [] then begin
      file_menu =:=    [
        "Open File", "load_buffer";
        "Save Buffer", "save_buffer";
        (*s: file menu entries *)
        "Kill Buffer", "kill_buffer";
        (*x: file menu entries *)
        "Compile", "compile";
        (*e: file menu entries *)
        "", "";
        "Quit", "exit";
      ]
    end;
  (*e: [[Std_efunsrc]] file menu setup *)
  (*s: [[Std_efunsrc]] edit menu setup *)
  if !!edit_menu = [] then begin
      edit_menu =:= [ 
        "Cut",    "kill_region";
        "Paste",  "insert_killed";

        "Undo",    "undo";
        "", "";
        (*s: [[edit_menu]] entries *)
        "Cut Frame", "v_cut_frame";
        (*x: [[edit_menu]] entries *)
        "One Frame", "one_frame";
        "Delete Frame", "delete_frame";
        (*e: [[edit_menu]] entries *)
      ];
    end;
  (*e: [[Std_efunsrc]] edit menu setup *)
  (*s: [[Std_efunsrc]] help menu setup *)
  help_menu := [|
    "Key Bindings", (fun frame ->
      Frame.change_buffer frame.frm_window "*bindings*"
    );
    "About Efuns", (fun frame ->
      Frame.change_buffer frame.frm_window "*help*"
    );
    "Changes", (fun frame ->
      (*
        let _ = Frame.load_file frame.frm_window (
            Version.efuns_lib ^"/Changes") in ()
      *)
      failwith "Std_xxx.menu changes: TODO"
    );

  |];
  (*e: [[Std_efunsrc]] help menu setup *)
  (*s: [[Std_efunsrc]] buffers menu setup *)
  buffers_menu := (fun top_window button () ->
      let buffers = ref [] in
      let loc = top_window.top_location in
      Hashtbl.iter (fun name _buf -> buffers := name :: !buffers) loc.loc_buffers;
      let _desc = Array.map (fun name -> 
            (name, wrap top_window (fun top_window ->
                  let frame = top_window.top_active_frame in
                  let window = frame.frm_window in
                  Frame.change_buffer window name
              )))
        (Array.of_list !buffers) in

      (* X11 *)
      (*
      let menu = new WX_popup.t top_window.top_root desc in
      let (x,y) = button#root_coordinates in
      menu#popup_once x (y + button#height) (Some !WX_types.button_event)
      *)
      failwith "Std_menu: show menus TODO"
      )
  (*e: [[Std_efunsrc]] buffers menu setup *)
(*e: toplevel Std_efunsrc._4 *)
  
(*s: toplevel Std_efunsrc._5 *)
let _ =
  Efuns.add_start_hook (fun location ->
      (*s: [[Std_efunsrc._5]] start hooks options *)
      add_option_parameter location compile_find_makefile;
      add_option_parameter location Text.add_amount;
      (*e: [[Std_efunsrc._5]] start hooks options *)
      init_global_map location
  )
(*e: toplevel Std_efunsrc._5 *)
(*e: std_efunsrc.ml *)
