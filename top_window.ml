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
open Xtypes
open WX_types
open Efuns
open Window

  
let message top_window msg =
  let xterm = Window.xterm top_window  in
  let len = String.length msg in
  WX_xterm.draw_string xterm 0 (top_window.top_height - 1)
    msg 0 len Text.direct_attr;
  WX_xterm.clear_eol xterm len (top_window.top_height - 1)
    (top_window.top_width - len);
  match top_window.top_mini_buffers with
    [] -> (* No mini-buffer ... *) ()
  | mini_buffer :: _ -> (* one mini-buffer is active *)
      match top_window.top_display with
        None -> ()
      | Some display ->
          WX_xterm.update_displays ();
          let _ = Unix.select [] [] [] 0.2 in
          mini_buffer.frm_redraw <- true

let clear_message top_window =
  match top_window.top_mini_buffers with
    [] -> 
      let xterm = Window.xterm top_window in
      WX_xterm.clear_eol xterm 0 
        (top_window.top_height - 1) top_window.top_width; 
  | _ -> ()



let dummy_action frame = ()

let try_map frame key =
  let prefix = frame.frm_prefix in
  let keylist = prefix @ [key] in
  match Ebuffer.get_binding frame.frm_buffer keylist with
    Unbound -> raise UnboundKey
  | Prefix map ->
      frame.frm_prefix <- keylist;
      let top_window = top frame.frm_window in
      message top_window (Keymap.print_key_list frame.frm_prefix);
  | Function f ->
      frame.frm_repeat_action <- 
      if (f == frame.frm_last_action) then
        frame.frm_repeat_action + 1
      else 0;
      frame.frm_prefix <- [];
      f frame;
      frame.frm_last_action <- f

let set_cursor_on top_window frame = 
  Frame.set_cursor frame;
  if frame.frm_cursor.[0] <> '\000' then
    let xterm = xterm top_window in
    WX_xterm.draw_string xterm 
      (frame.frm_xpos + frame.frm_cursor_x-frame.frm_x_offset) 
    (frame.frm_ypos + frame.frm_cursor_y) 
    frame.frm_cursor 0 1 Text.inverse_attr

let set_cursor_off top_window frame =
  if frame.frm_cursor.[0] <> '\000' then
    let xterm = xterm top_window in
    WX_xterm.draw_string xterm 
      (frame.frm_xpos + frame.frm_cursor_x) 
    (frame.frm_ypos + frame.frm_cursor_y) 
    frame.frm_cursor 0 1 frame.frm_cursor_attr

let cursor_on top_window =
  let frame = top_window.top_active_frame in
  let name = frame.frm_buffer.buf_name in
  if not (name == top_window.top_name) then
    begin
      top_window.top_appli#setWM_NAME name;
      top_window.top_name <- name
    end;
  set_cursor_on top_window frame;
  match top_window.top_second_cursor with
    None -> ()
  | Some frame -> set_cursor_on top_window frame

let cursor_off top_window =
  let frame = top_window.top_active_frame in
  set_cursor_off top_window frame;
  match top_window.top_second_cursor with
    None -> ()
  | Some frame -> set_cursor_off top_window frame


let update_display location =
  List.iter (fun top_window -> 
      iter (Frame.update top_window) top_window.top_windows;
      (match top_window.top_mini_buffers with
          [] -> ()
        | frame :: _ ->
            Frame.update top_window frame);
      cursor_on top_window;
  ) location.loc_windows

let clean_display location =
  List.iter (fun top_window -> 
      cursor_off top_window
  ) location.loc_windows

let rec resize_window window xpos ypos width height =
  let old_width = window.win_width in
  let old_height = window.win_height in
  window.win_xpos <- xpos;
  window.win_ypos <- ypos;
  window.win_width <- width;
  window.win_height <- height;
  match window.win_down with
    WFrame frame -> Frame.install window frame
  | NoFrame () -> assert false
  | HComb (w1,w2) ->
      let wi1 = w1.win_width * width / old_width in
      resize_window w1 xpos ypos wi1 height;
      resize_window w2 (xpos + wi1) ypos (width - wi1) height
  | VComb (w1,w2) ->
      let he1 = w1.win_height * height / old_height in
      resize_window w1 xpos ypos width he1;
      resize_window w2 xpos (ypos + he1) width (height - he1)

let rec find_frame window x y =
  match window.win_down with
    NoFrame () -> assert false
  | WFrame frame -> frame
  | HComb (w1,w2) -> 
      if w2.win_xpos > x then find_frame w1 x y
      else find_frame w2 x y
  | VComb (w1,w2) ->
      if w2.win_ypos > y then find_frame w1 x y
      else find_frame w2 x y

let keypressed = ref 0
let mouse_x = ref 0
let mouse_y = ref 0

let find_selected_frame top_window =
  let x = !mouse_x in
  let y = !mouse_y in
  let frame = 
    if y > top_window.top_height - 2 then
      match top_window.top_mini_buffers with
        [] -> raise Not_found
      | mini_frame :: _ -> mini_frame
    else
      find_frame top_window.top_windows x y
  in
  Frame.active frame;
  frame


let mouse_set_active top_window =
  let x = !mouse_x in
  let y = !mouse_y in
  let frame = find_selected_frame top_window in
  Frame.move_point frame frame.frm_point x y;
  frame


let mini_message frame msg =
  let top_window = Window.top frame.frm_window in
  message top_window msg

let handle_key_start_hook = Local.create_abstr "handle_key_start_hook"
let handle_key_end_hook = Local.create_abstr "handle_key_end_hook"
  
let meta = ref Xtypes.mod1Mask
  
let handle_key top_window modifiers keysym =
  keypressed := keysym;
  let location = top_window.top_location in
  let frame = top_window.top_active_frame in
  clean_display location;
  clear_message top_window;
  exec_hooks (try get_global location handle_key_start_hook with _ -> []) location;
  begin
    let mods = 
      let mask = Xtypes.controlMask lor !meta in
      let diff = modifiers land mask in
      if diff = Xtypes.controlMask then ControlMap else
      if diff = !meta then MetaMap else
      if diff = 0 then NormalMap else ControlMetaMap in
    let key = (mods, keysym) in
    try
      try_map frame key
    with
      UnboundKey -> 
        message top_window
          (Printf.sprintf "Unbound key %s %s"
            (Keymap.print_key_list frame.frm_prefix)
          (Keymap.print_key key));
        frame.frm_prefix <- [];
    | Failure str -> message top_window str
    | e -> message top_window 
          (Printf.sprintf "Uncaught exception %s" (Utils.printexn e))
  end;
  exec_hooks (try get_global location handle_key_end_hook with _ -> []) location;
  update_display top_window.top_location

  (* We can receive events from different sources. In particular, some of
  them can be received during the painting (scrollbar ...)
  *)
  
let wrap top_window f () = 
  let location = top_window.top_location in
  Concur.Mutex.lock location.loc_mutex;  
  clean_display location;    
  clear_message top_window;
  keypressed := XK.xk_Menu;
  exec_hooks
    (try get_global location handle_key_start_hook with _ -> []) location;    
  begin
    try f top_window with e ->   
        message top_window 
          (Printf.sprintf "Uncaught exception %s" (Utils.printexn e))
  end;
  exec_hooks 
    (try get_global location handle_key_end_hook with _ -> []) location;    
  update_display top_window.top_location;
  WX_xterm.update_displays ();
  Concur.Mutex.unlock top_window.top_location.loc_mutex

let wrap_item top_window (n,f) =
  n, wrap top_window (fun top_window -> f top_window.top_active_frame)
        
let handler top_window xterm event =
  let location = top_window.top_location in
  Concur.Mutex.lock location.loc_mutex;
  try
    begin
      match event with
        WX_xterm.XTKeyPress (modifiers, s, keysym) ->
          if (keysym < XK.xk_Shift_L || keysym > XK.xk_Hyper_R)
          then 
            handle_key top_window modifiers keysym
      
      | WX_xterm.XTButtonPress (modifiers,button,x,y) -> 
          mouse_x := x;
          mouse_y := y;
          handle_key top_window modifiers (XK.xk_Pointer_Button_Dflt + button)
      
      | WX_xterm.XTMouseMotion (modifiers,button,x,y) ->
          mouse_x := x;
          mouse_y := y;
          handle_key top_window modifiers (XK.xk_Pointer_Drag_Dflt + button)
      
      | WX_xterm.XTResize (new_width, new_height) ->
          resize_window top_window.top_windows 0 0 new_width (new_height - 1);
          List.iter
            (fun frame -> 
              let window = frame.frm_window in
              window.win_ypos <- new_height - 1;
              window.win_width <- new_width;
              Frame.install window frame) top_window.top_mini_buffers;
          top_window.top_width <- new_width;
          top_window.top_height <- new_height;
          clear_message top_window;
          update_display top_window.top_location
          
    end;
    Concur.Mutex.unlock top_window.top_location.loc_mutex;
  with
    e ->   
      Concur.Mutex.unlock top_window.top_location.loc_mutex;
      raise e


let buffers_menu = ref (fun (top_window : top_window) (button : WX_button.t) ()
      -> ())

let scroll_to_frame ady top_window =
  let frame = top_window.top_active_frame in
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let pos_start = Text.get_position text frame.frm_start in
  let pos_end = Text.get_position text frame.frm_end in
  let size = Text.size text in
  let y = ady#get_pos size in
  if abs (y - pos_start) > 80 then
  (* the position has changed *)
    begin
      frame.frm_force_start <- true;
      frame.frm_redraw <- true;
      Text.set_position text frame.frm_start y
    end
(*
    Text.set_position text point y;
      let point = Text.add_point text in
      let curline = Text.point_line text frame.frm_start in
      let newline = Text.point_line text point in
      Text.remove_point text point;
    frame.frm_y_offset <- frame.frm_y_offset + (newline - curline);
  *)

let menus = define_option ["menus"] ""
    (list_option (tuple2_option (string_option, list_option string2_option)))
  []
let file_menu = define_option ["file_menu"] ""
    (list_option string2_option) []
let edit_menu = define_option ["edit_menu"] ""
    (list_option string2_option) []
let help_menu = ref [| |]

  
  
let create location display =
  let buf = Ebuffer.default location "*help*" in
  let top = new WX_appli.t display.WX_xterm.root_oo [] in
  top#setWM_NAME "new_frame";
  top#setWM_CLASS "Efuns" "efuns";
  let hbar = new WX_bar.h top#container [] in
  top#container_add hbar#contained;
  let xterm = new WX_xterm.t 
      hbar#container display
      location.loc_width
      location.loc_height in
  let ady = new WX_adjust.t () in
  let scrollbar = new WX_scrollbar.v hbar#container ady [] in
  hbar#container_add_s [xterm#contained; scrollbar#contained];
  let window = Window.create_at_top  0 0 location.loc_width (location.loc_height - 1) in
  let frame = Frame.create_without_top location window None buf  in
  let top_window =
    { top_location = location;
      top_display = Some display;
      top_xterm = None;
      top_term = xterm;
      top_attrs = Array.create 256 None;
      top_windows = window;
      top_mini_buffers = [];
      top_width = location.loc_width;
      top_height = location.loc_height;
      top_name = "window";
      top_active_frame = frame;
      top_second_cursor = None;
      top_root=  display.WX_xterm.root_oo;
      top_appli = top;
      top_scrollbar = ady;
    } 
  in
  ady#add_subject (fun () ->
      let frame = top_window.top_active_frame in
      if not frame.frm_force_start then
        wrap top_window (scroll_to_frame ady) ()); 
  frame.frm_window.win_up <- TopWindow top_window;
  location.loc_windows <- top_window :: location.loc_windows;
  top#add_button "Buffers" (!buffers_menu top_window);
  top#add_menu "File" (Array.map (fun (name,action) ->
        wrap_item top_window (name, execute_action action)
    ) (Array.of_list !!file_menu));
  top#add_menu "Edit" (Array.map (fun (name,action) ->
        wrap_item top_window (name, execute_action action)
    ) (Array.of_list !!edit_menu));
  List.iter (fun (menu_name, items) ->
      top#add_menu menu_name 
        (Array.map (fun (name,action) ->
            wrap_item top_window (name, execute_action action)
        ) (Array.of_list items))
  ) !!menus;
  top#add_separator;
  top#add_menu "Help" (Array.map (wrap_item top_window) !help_menu);
  top#show;
  let xterm = xterm#xterm in
  top_window.top_xterm <- Some xterm;
  WX_xterm.install_handler display xterm (handler top_window xterm);
  top#configure [Bindings [Key (anyKey, anyModifier), (fun _ ->
          handler top_window xterm (WX_xterm.XTKeyPress (
              !WX_types.modifiers_event, !key_string, !key_sym));
          WX_xterm.update_displays ()
    )]];
  top_window

let delete_window frame =
  let top_window = Window.top frame.frm_window in
  let location = top_window.top_location in
  if List.length location.loc_windows > 1 then
    let xterm = Window.xterm top_window in
    top_window.top_appli#destroy;
    WX_xterm.destroy_window xterm;
    Frame.kill_all top_window.top_windows;
    location.loc_windows <- Utils.list_remove location.loc_windows
      top_window
    
    

        
let check_abort = ref 0
let check_abort_delay = ref 100

  (* This function should be used in loops that could be interrupted by
  the user. It returns "true" if the user pressed C-g, and false
  in other cases. *)
  
let check_abort frame =
  incr check_abort; 
  if !check_abort mod !check_abort_delay = 0 then
    let top_window = Window.top frame.frm_window in
    let xterm = top_window.top_term in
    xterm#check_abort
  else false
