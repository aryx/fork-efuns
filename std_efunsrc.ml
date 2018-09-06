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
open Efuns

open Keymap (* c_xxx *)
(*s: constant [[standard_map]] *)
let standard_map = [
  (*s: [[standard_map]] entries *)
  (* -------------------------------------------------------- *)
  (* Loading *)
  (* -------------------------------------------------------- *)
  (*s: loading keys *)
  [c_x; ControlMap, Char.code 'f'], Complexe.load_buffer;
  (*x: loading keys *)
  [c_x; NormalMap, Char.code 'i'], Complexe.insert_file;
  (*e: loading keys *)

  (* -------------------------------------------------------- *)
  (* Navigating (in the file) *)
  (* -------------------------------------------------------- *)
  (*s: navigating keys *)
  [NormalMap, XK.xk_Left], (fun frm -> ignore (Simple.move_backward frm 1)); 
  [NormalMap, XK.xk_Right], (fun frm -> ignore (Simple.move_forward frm 1)); 
  [ControlMap, Char.code 'b'], (fun frm -> ignore (Simple.move_backward frm 1)); 
  [ControlMap, Char.code 'f'], (fun frm -> ignore (Simple.move_forward frm 1)); 

  [ControlMap, XK.xk_Left ], (Frame.to_frame Simple.backward_word);
  [ControlMap, XK.xk_Right ], (Frame.to_frame Simple.forward_word);  
  [MetaMap, XK.xk_Left ], (Frame.to_frame Simple.backward_word);
  [MetaMap, XK.xk_Right ], (Frame.to_frame Simple.forward_word);

  [ControlMap, Char.code 'a'], Simple.beginning_of_line;
  [ControlMap, Char.code 'e'], Simple.end_of_line;

  [NormalMap, XK.xk_Up], Simple.backward_line; 
  [NormalMap, XK.xk_Down], Simple.forward_line; 


  [ControlMap, XK.xk_Up], (Frame.to_frame Simple.backward_paragraph);
  [ControlMap, XK.xk_Down], (Frame.to_frame Simple.forward_paragraph);  

  [NormalMap, XK.xk_Prior], Simple.backward_screen; 
  [NormalMap, XK.xk_Next], Simple.forward_screen;

  [ControlMap, XK.xk_Next], Simple.end_of_file;
  [ControlMap, XK.xk_Prior], Simple.begin_of_file;
  [MetaMap, Char.code '>'], Simple.end_of_file;
  [MetaMap, Char.code '<'], Simple.begin_of_file;
  (*x: navigating keys *)
  [ControlMap, Char.code 'u'; ControlMap, Char.code ' '], 
    Simple.goto_last_saved_pos; 
  (*e: navigating keys *)

  (* -------------------------------------------------------- *)
  (* Editing *)
  (* -------------------------------------------------------- *)

  (* ------------- *)
  (* Inserting *)
  (* ------------- *)
  (* see also the start_hook in simple.ml setting many self_insert_cmd *)
  (*s: inserting keys *)
  [NormalMap, XK.xk_Return], Simple.insert_return; 
  (*e: inserting keys *)
  (* ------------- *)
  (* Deleting *)
  (* ------------- *)
  (*s: deleting keys *)
  [NormalMap, XK.xk_BackSpace], Simple.delete_backspace_char; 
  [ControlMap, Char.code 'd'], Simple.delete_char;
  [NormalMap, XK.xk_Delete], Simple.delete_char; 

  [MetaMap, Char.code 'd' ], (Frame.to_frame Simple.delete_forward_word);
  [MetaMap, XK.xk_BackSpace ], (Frame.to_frame Simple.delete_backward_word);

  [ControlMap, Char.code 'k'], Simple.kill_end_of_line;
  (*x: deleting keys *)
  [ControlMap, XK.xk_BackSpace], Simple.hungry_electric_delete;
  (*e: deleting keys *)
  (* ------------------------------ *)
  (* Moving (Cut, copy, paste) *)
  (* ------------------------------ *)
  (*s: moving keys *)
  [ControlMap, Char.code ' '], Complexe.mark_at_point;
  [ControlMap, Char.code 'w'], Simple.kill_region;
  [ControlMap, Char.code 'y'], Simple.insert_killed;
  [MetaMap, Char.code 'y'], Simple.insert_next_killed;
  [MetaMap, Char.code 'w'], Simple.copy_region;
  (*e: moving keys *)
  (* ---------------------- *)
  (* Transforming *)
  (* ---------------------- *)
  (*s: transforming keys *)
  [ControlMap, Char.code 't'], (Frame.to_frame Simple.transpose_chars);
  [MetaMap, Char.code 't'], (Frame.to_frame Simple.transpose_words);

  [MetaMap, Char.code 'l'], (fun frm ->
    Simple.on_word frm.frm_buffer frm.frm_point String.lowercase
  );
  [MetaMap, Char.code 'u'], (fun frm ->
    Simple.on_word frm.frm_buffer frm.frm_point String.uppercase
  );
  [MetaMap, Char.code 'c'], (fun frm ->
    Simple.on_word frm.frm_buffer frm.frm_point String.capitalize
  );
  [MetaMap, XK.xk_q], Simple.fill_paragraph;
  (*e: transforming keys *)

  (* -------------------------------------------------------- *)
  (* Search/replace *)
  (* -------------------------------------------------------- *)
  (*s: searching keys *)
  [ControlMap, Char.code 's'], Search.isearch_forward;
  [ControlMap, Char.code 'r'], Search.isearch_backward;
  [MetaMap, Char.code 's'], Search.isearch_forward_regexp;
  [MetaMap, Char.code 'r'], Search.isearch_backward_regexp;
  (*e: searching keys *)
  (*s: replacing keys *)
  [MetaMap, Char.code '%'], Search.query_replace_string;
  (*e: replacing keys *)

  (* -------------------------------------------------------- *)
  (* Undoing *)
  (* -------------------------------------------------------- *)
  (*s: undoing keys *)
  [ControlMap, Char.code '_'], Simple.undo;
  (*e: undoing keys *)

  (* -------------------------------------------------------- *)
  (* External commands *)
  (* -------------------------------------------------------- *)
  (*s: external commands keys *)
  [MetaMap, Char.code '!'], System.shell_command;
  (*e: external commands keys *)

  (* -------------------------------------------------------- *)
  (* Buffers/windows/frames *)
  (* -------------------------------------------------------- *)
  (*s: buffer management keys *)
  [c_x; NormalMap, Char.code 'b'], Multi_buffers.change_buffer;
  (*x: buffer management keys *)
  [c_x; NormalMap, Char.code 'k'], Multi_buffers.kill_buffer;
  (*e: buffer management keys *)
  (*s: buffer navigating keys *)
  (* pinning *)
  [ControlMetaMap, XK.xk_Down], Multi_buffers.down_buffer;
  [ControlMetaMap, XK.xk_Up], Multi_buffers.up_buffer;
  (*e: buffer navigating keys *)
  (*s: frame management keys *)
  [c_x; NormalMap, Char.code '2'], Multi_frames.v_cut_frame;    
  [c_x; NormalMap, Char.code '3'], Multi_frames.h_cut_frame;    
  (*x: frame management keys *)
  [c_x; NormalMap, Char.code '1'], Multi_frames.one_frame;
  [c_x; NormalMap, Char.code '0'], Multi_frames.delete_frame;
  (*e: frame management keys *)
  (*s: frame navigation keys *)
  [c_x; NormalMap, Char.code 'o'], Multi_frames.next_frame;
  (*e: frame navigation keys *)

  (* -------------------------------------------------------- *)
  (* Meta *)
  (* -------------------------------------------------------- *)
  (*s: meta keys *)
  [MetaMap, Char.code 'x'], Interactive.call_interactive;
  (*e: meta keys *)

  (* -------------------------------------------------------- *)
  (* Saving *)
  (* -------------------------------------------------------- *)
  (*s: saving keys *)
  [c_x; ControlMap, Char.code 's'], Complexe.save_buffer; 
  (*x: saving keys *)
  [c_x; NormalMap,  Char.code 's'], Complexe.save_some_buffers;
  [c_x; ControlMap, Char.code 'w'], Complexe.write_buffer; 
  (*e: saving keys *)

  (* -------------------------------------------------------- *)
  (* Misc *)
  (* -------------------------------------------------------- *)
  (*s: misc keys *)
  [c_h; NormalMap, Char.code 'v'], Complexe.describe_variable;
  (*x: misc keys *)
  [c_x; NormalMap, Char.code '='], Complexe.cursor_position;
  (*x: misc keys *)
  [c_x; NormalMap, Char.code 'F'], Complexe.change_font;
  (*x: misc keys *)
  [c_x; ControlMap, Char.code 'c'], Complexe.exit_efuns; 
  (*x: misc keys *)
  [ControlMap, Char.code 'l'], Simple.recenter;
  (*x: misc keys *)
  [c_h; NormalMap, Char.code 'K'], Frame.bindings_help;
  (*x: misc keys *)
  [c_x; ControlMap, Char.code 'x'], Simple.point_at_mark;
  (*x: misc keys *)
  [ c_c; NormalMap, Char.code '-'], Structure.next_hole;
  (*x: misc keys *)
  [c_x;NormalMap, Char.code '`' ], Compil.next_error;
  (*x: misc keys *)
  [NormalMap, Char.code ' '], Abbrevs.char_expand_abbrev;
  [MetaMap, Char.code '/'], Abbrevs.dabbrev_expand;
  (*x: misc keys *)
  (*s: window management keys *)
  (* less: delete *)
  [c_x; n_5; NormalMap, Char.code 'f'], Complexe.window_load_buffer;
  [c_x; n_5; NormalMap, Char.code 'b'], Complexe.window_change_buffer;
  [c_x; n_5; NormalMap, Char.code '0'], Top_window.delete_window;
  (*e: window management keys *)
  (*x: misc keys *)
  [NormalMap, XK.xk_Insert], Simple.toggle_overwrite_mode;
  (*e: misc keys *)
  (*e: [[standard_map]] entries *)
]
(*e: constant [[standard_map]] *)

(*s: constant [[Std_efunsrc.grep_hist]] *)
let grep_hist = ref ["grep -n "]
(*e: constant [[Std_efunsrc.grep_hist]] *)

(*s: function [[Std_efunsrc.save_options]] *)
let save_options frame = 
  Options.save ()
(*e: function [[Std_efunsrc.save_options]] *)
(*s: function [[Std_efunsrc.fondamental_mode]] *)
let fondamental_mode frame =
  Ebuffer.set_major_mode frame.frm_buffer Ebuffer.fondamental_mode
(*e: function [[Std_efunsrc.fondamental_mode]] *)

open Options
  
(*s: constant [[Std_efunsrc.global_map]] *)
let global_map = define_option ["global_map"] "" 
  (list_option Simple.binding_option) []
(*e: constant [[Std_efunsrc.global_map]] *)
(*s: constant [[Std_efunsrc.interactives_map]] *)
let interactives_map = define_option ["interactives_map"] ""
    (list_option string2_option) 
  []
(*e: constant [[Std_efunsrc.interactives_map]] *)

(*s: function [[Std_efunsrc.init_global_map]] *)
let init_global_map () = 
  if !!global_map = [] 
  then 
    standard_map |> List.iter (fun (keys, action) ->
        Keymap.add_global_key keys "" action
    )
  else 
  !!global_map |> List.iter (fun (keys, action) ->
      try
        Keymap.add_global_key keys action (Action.execute_action action)
      with e ->
        Log.printf "Error for action %s" action;
        Log.exn "%s\n" e;
  );

  (*s: [[Std_efunsrc.init_global_map()]] add interactives from [[interactives_map]] *)
  !!interactives_map |> List.iter (fun (name, action) ->
    try
      Keymap.add_interactive (Globals.editor()).edt_map name 
        (Action.execute_action action)
    with e ->
      Log.printf "Error for action %s" action;
      Log.exn "%s\n" e;
  );
  (*e: [[Std_efunsrc.init_global_map()]] add interactives from [[interactives_map]] *)
    

  (* Mouse *)
  (*s: [[Std_efunsrc.init_global_map()]] mouse keys setup *)
  Keymap.add_global_key [NormalMap, XK.xk_Pointer_Button1]
    "set_active_frame" Mouse.mouse_set_frame;
  (*x: [[Std_efunsrc.init_global_map()]] mouse keys setup *)
  Keymap.add_global_key [NormalMap, XK.xk_Pointer_Button3]
    "mouse_save_then_kill" Mouse.mouse_save_then_kill;
  Keymap.add_global_key [NormalMap, XK.xk_Pointer_Button2]
    "insert_at_point" Mouse.mouse_yank_at_click;
  (*e: [[Std_efunsrc.init_global_map()]] mouse keys setup *)
  ()
(*e: function [[Std_efunsrc.init_global_map]] *)
  
(*s: toplevel [[Std_efunsrc]] menu settings *)
let _ =
  (*s: [[Std_efunsrc]] file menu setup *)
  if !!Top_window.file_menu = [] then begin
    Top_window.file_menu =:= [
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
  if !!Top_window.edit_menu = [] then begin
      Top_window.edit_menu =:= [ 
        "Cut",    "kill_region";
        "Paste",  "insert_killed";

        "Undo",    "undo";
        "", "";
        (*s: [[edit_menu]] entries *)
        "Cut Frame", "vertical_cut_frame";
        (*x: [[edit_menu]] entries *)
        "One Frame", "one_frame";
        "Delete Frame", "delete_frame";
        (*e: [[edit_menu]] entries *)
      ];
    end;
  (*e: [[Std_efunsrc]] edit menu setup *)
  (*s: [[Std_efunsrc]] help menu setup *)
  Top_window.help_menu := [|
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
  Top_window.buffers_menu := (fun top_window button () ->
      let buffers = ref [] in
      let edt = Globals.editor() in
      Hashtbl.iter (fun name _buf -> buffers := name :: !buffers) edt.edt_buffers;
      let _desc = Array.map (fun name -> 
            (name, Top_window.wrap top_window (fun top_window ->
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
(*e: toplevel [[Std_efunsrc]] menu settings *)
  
(*s: toplevel [[Std_efunsrc]] starting hook *)
let _ =
  Hook.add_start_hook (fun () ->
    (*s: [[Std_efunsrc._5]] start hooks options *)
    Parameter.add_option_parameter Compil.compile_find_makefile;
    Parameter.add_option_parameter Text.add_amount;
    (*e: [[Std_efunsrc._5]] start hooks options *)
    init_global_map ();
    Hook.add_hook Top_window.handle_key_start_hook Complexe.check_file;      
  )
(*e: toplevel [[Std_efunsrc]] starting hook *)
(*e: std_efunsrc.ml *)
