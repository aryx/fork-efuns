(*s: graphics/top_window.ml *)
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
open Common
open Options
open Efuns

  
(*s: function Top_window.message *)
let message top_window msg =
  let graphic = Efuns.backend top_window in
  let len = String.length msg in

  graphic.Xdraw.draw_string 0 (top_window.top_height - 1)
    msg 0 len Text.direct_attr;
  graphic.Xdraw.clear_eol len (top_window.top_height - 1)
    (top_window.top_width - len);

  match top_window.top_mini_buffers with
  | [] -> (* No mini-buffer ... *) ()
  | mini_buffer :: _ -> (* one mini-buffer is active *)
      graphic.Xdraw.update_display ();
      (* let _ = Unix.select [] [] [] 0.2 in *)
      mini_buffer.frm_redraw <- true
(*e: function Top_window.message *)

(*s: function Top_window.clear_message *)
let clear_message top_window =
  match top_window.top_mini_buffers with
  | [] -> 
      let graphic = Efuns.backend top_window in
      graphic.Xdraw.clear_eol 
        0 (top_window.top_height - 1) 
        top_window.top_width; 
  | _ -> ()
(*e: function Top_window.clear_message *)

(*s: function Top_window.try_map *)
let try_map frame key =
  let keylist = frame.frm_prefix @ [key] in
  match Ebuffer.get_binding frame.frm_buffer keylist with
  | Function f ->
      frame.frm_prefix <- [];
      (* dispatch the action *)
      (*s: [[Top_window.try_map()]] if debug, print action name *)
      if !Globals.debug
      then begin
        let found = ref false in
        Action.actions |> Hashtbl.iter  (fun k v ->
          match v with
          (* subtle: this will work only if f2 was not a closure *)
          | FrameAction f2 when f == f2 ->
              found := true;
              pr2 (spf "action: %s" k)
          | _ -> ()
        );
        if not !found
        then pr2 ("action not found");
      end;
      (*e: [[Top_window.try_map()]] if debug, print action name *)
      f frame; 
      (*s: [[Top_window.try_map()]] set last action *)
      frame.frm_last_action <- f
      (*e: [[Top_window.try_map()]] set last action *)
  | Prefix map ->
      frame.frm_prefix <- keylist;
      let top_window = Window.top frame.frm_window in
      message top_window (Keymap.print_key_list frame.frm_prefix);
  | Unbound -> raise UnboundKey
(*e: function Top_window.try_map *)

(*s: function Top_window.set_cursor_on *)
let set_cursor_on top_window frame = 
  Frame.set_cursor frame;

  if frame.frm_cursor.[0] <> '\000' then
    let graphic = Efuns.backend top_window in
    graphic.Xdraw.draw_string
      (frame.frm_xpos + frame.frm_cursor_x-frame.frm_x_offset)
      (frame.frm_ypos + frame.frm_cursor_y) 
      frame.frm_cursor 0 1 Text.inverse_attr
(*e: function Top_window.set_cursor_on *)

(*s: function Top_window.set_cursor_off *)
let set_cursor_off top_window frame =
  if frame.frm_cursor.[0] <> '\000' then
    let graphic = Efuns.backend top_window in
    graphic.Xdraw.draw_string
      (frame.frm_xpos + frame.frm_cursor_x) 
      (frame.frm_ypos + frame.frm_cursor_y) 
      frame.frm_cursor 0 1 frame.frm_cursor_attr
(*e: function Top_window.set_cursor_off *)

(*s: function Top_window.cursor_on *)
let cursor_on top_window =
  let frame = top_window.top_active_frame in
  set_cursor_on top_window frame;
  (*s: [[Top_window.cursor_on()]] set top window name *)
  let name = frame.frm_buffer.buf_name in
  if name <> top_window.top_name then begin
    let graphic = Efuns.backend top_window in
    graphic.Xdraw.update_window_title name;
    top_window.top_name <- name
  end;
  (*e: [[Top_window.cursor_on()]] set top window name *)
  (*s: [[Top_window.cursor_on()]] display secondary cursor *)
  top_window.top_second_cursor |> Common.do_option (fun frame ->
    set_cursor_on top_window frame
  )
  (*e: [[Top_window.cursor_on()]] display secondary cursor *)
(*e: function Top_window.cursor_on *)

(*s: function Top_window.cursor_off *)
let cursor_off top_window =
  let frame = top_window.top_active_frame in
  set_cursor_off top_window frame;
  (*s: [[Top_window.cursor_off()]] hide secondary cursor *)
  top_window.top_second_cursor |> Common.do_option (fun frame ->
    set_cursor_off top_window frame
  )
  (*e: [[Top_window.cursor_off()]] hide secondary cursor *)
(*e: function Top_window.cursor_off *)


(*s: function Top_window.update_display *)
let update_display () =
  (Globals.location()).top_windows |> List.iter (fun top_window ->
     top_window.window |> Window.iter (fun frm -> 
       Frame.display top_window frm
     );
     (match top_window.top_mini_buffers with
      | [] -> ()
      | frm :: _ -> Frame.display top_window frm
     );
     cursor_on top_window;
     let graphic = Efuns.backend top_window in
     graphic.Xdraw.update_display();
  ) 
(*e: function Top_window.update_display *)
let update_display () = Common.profile_code "Top_window.update_display" 
  (fun () -> update_display () )

(*s: function Top_window.clean_display *)
let clean_display () =
  (Globals.location()).top_windows |> List.iter (fun top_window ->
     cursor_off top_window;
     (*s: [[clean_display()]] sanity check second cursor and minibuffer *)
     (match top_window.top_mini_buffers, top_window.top_second_cursor with
      | _, None -> ()
      | _::_, Some _ -> ()
      | [], Some frame ->
          Globals.error "weird, second cursor for %s but no minibuffer"
            (frame.frm_buffer.buf_name);
          top_window.top_second_cursor <- None
     )
     (*e: [[clean_display()]] sanity check second cursor and minibuffer *)
   )
(*e: function Top_window.clean_display *)

(*s: function Top_window.resize_window *)
let rec resize_window window xpos ypos width height =
  let old_width = window.win_width in
  let old_height = window.win_height in
  window.win_xpos <- xpos;
  window.win_ypos <- ypos;
  window.win_width <- width;
  window.win_height <- height;
  match window.win_down with
    WFrame frame -> Frame.install window frame
  | NoFrame -> assert false
  | HComb (w1,w2) ->
      let wi1 = w1.win_width * width / old_width in
      resize_window w1 xpos ypos wi1 height;
      resize_window w2 (xpos + wi1) ypos (width - wi1) height
  | VComb (w1,w2) ->
      let he1 = w1.win_height * height / old_height in
      resize_window w1 xpos ypos width he1;
      resize_window w2 xpos (ypos + he1) width (height - he1)
(*e: function Top_window.resize_window *)

(*s: function Top_window.find_frame *)
let rec find_frame window x y =
  match window.win_down with
    NoFrame -> raise Not_found
  | WFrame frame -> frame
  | HComb (w1,w2) -> 
      if w2.win_xpos > x 
      then find_frame w1 x y
      else find_frame w2 x y
  | VComb (w1,w2) ->
      if w2.win_ypos > y 
      then find_frame w1 x y
      else find_frame w2 x y
(*e: function Top_window.find_frame *)

(*s: constant Top_window.keypressed *)
let keypressed = ref 0
(*e: constant Top_window.keypressed *)
(*s: constant Top_window.mouse_x *)
let mouse_x = ref 0
(*e: constant Top_window.mouse_x *)
(*s: constant Top_window.mouse_y *)
let mouse_y = ref 0
(*e: constant Top_window.mouse_y *)

(*s: function Top_window.find_selected_frame *)
let find_selected_frame top_window =
  let x = !mouse_x in
  let y = !mouse_y in
  let frame = 
    if y > top_window.top_height - 2 then
      match top_window.top_mini_buffers with
      | [] -> raise Not_found
      | mini_frame :: _ -> mini_frame
    else
      find_frame top_window.window x y
  in
  Frame.active frame;
  frame
(*e: function Top_window.find_selected_frame *)


(*s: function Top_window.mouse_set_active *)
let mouse_set_active top_window =
  let x = !mouse_x in
  let y = !mouse_y in
  let frame = find_selected_frame top_window in
  Frame.move_point frame frame.frm_point x y;
  frame
(*e: function Top_window.mouse_set_active *)


(*s: function Top_window.mini_message *)
let mini_message frame msg =
  let top_window = Window.top frame.frm_window in
  message top_window msg
(*e: function Top_window.mini_message *)

(*s: constant Top_window.handle_key_start_hook *)
let handle_key_start_hook = Local.create_abstr "handle_key_start_hook"
(*e: constant Top_window.handle_key_start_hook *)
(*s: constant Top_window.handle_key_end_hook *)
let handle_key_end_hook = Local.create_abstr "handle_key_end_hook"
(*e: constant Top_window.handle_key_end_hook *)
  
(*s: constant Top_window.meta *)
let meta = ref Xtypes.mod1Mask
(*e: constant Top_window.meta *)
  
(*s: function Top_window.handle_key *)
let handle_key top_window modifiers keysym =
  keypressed := keysym;
  let frame = top_window.top_active_frame in
  let buf = frame.frm_buffer in

  clean_display (); (* set cursor off *)
  clear_message top_window;

  Hook.exec_hooks(try Var.get_var buf handle_key_start_hook with _ ->[]) frame;

  let mod_ = 
    (*s: [[Top_window.handle_key()]] compute mod *)
    let mask = Xtypes.controlMask lor !meta in
    let diff = modifiers land mask in
    match () with
    | _ when diff = 0 -> NormalMap 
    | _ when diff = Xtypes.controlMask -> ControlMap
    | _ when diff = !meta -> MetaMap
    | _ -> ControlMetaMap
    (*e: [[Top_window.handle_key()]] compute mod *)
  in
  let key = (mod_, keysym) in
  begin
    try
      (* should lead to an action being triggered and modifying things! *)
      try_map frame key
    with
    (*s: [[Top_window.handle_key()]] handle exception of try_map *)
    | UnboundKey -> 
        message top_window
          (Printf.sprintf "Unbound key %s"
            (Keymap.print_key_list (frame.frm_prefix @ [key])));
        frame.frm_prefix <- [];
    | Failure str -> message top_window str
    | (Common.UnixExit _) as x -> raise x
    (*x: [[Top_window.handle_key()]] handle exception of try_map *)
    | e -> 
        let str = spf "Uncaught exception %s" (Utils.printexn e) in
        let bt = Printexc.get_backtrace () in
        if !Globals.debug
        then pr2 str;
        message top_window  str;
        let buf = Ebuffer.default "*backtrace*" in
        let text = buf.buf_text in
        Text.insert_at_end text str;
        Text.insert_at_end text "\n";
        Text.insert_at_end text bt;
        Text.insert_at_end text "\n"
    (*e: [[Top_window.handle_key()]] handle exception of try_map *)
  end;

  Hook.exec_hooks (try Var.get_global handle_key_end_hook with _ -> []) ();

  update_display () (* will set cursor back on and many other things *)
(*e: function Top_window.handle_key *)

  (* We can receive events from different sources. In particular, some of
  them can be received during the painting (scrollbar ...)
  *)
  
(*s: function Top_window.wrap *)
let wrap top_window f () = 
  let loc = Globals.location() in
  Mutex.lock loc.loc_mutex;  
  clean_display ();    
  clear_message top_window;
  keypressed := XK.xk_Menu;
  let frame = top_window.top_active_frame in
  Hook.exec_hooks (try Var.get_global handle_key_start_hook with _ ->[]) frame;
  begin
    try f top_window 
    with e -> message top_window (Printf.sprintf "Uncaught exception %s" 
                                    (Utils.printexn e))
  end;
  Hook.exec_hooks (try Var.get_global handle_key_end_hook with _ -> []) ();    
  update_display ();
  Mutex.unlock loc.loc_mutex
(*e: function Top_window.wrap *)

(*s: function Top_window.wrap_item *)
(*
let wrap_item top_window (n,f) =
  n, wrap top_window (fun top_window -> f top_window.top_active_frame)
*)
(*e: function Top_window.wrap_item *)
        
(*s: function Top_window.handler *)
let handler top_window event =
  Globals.with_lock (fun () ->
    match event with
    (*s: [[Top_window.handler()]] match event cases *)
    | Xtypes.XTKeyPress (modifiers, _s, keysym) ->
        handle_key top_window modifiers keysym
    (*x: [[Top_window.handler()]] match event cases *)
    | Xtypes.XTButtonPress (modifiers,button,x,y) -> 
        mouse_x := x;
        mouse_y := y;
        handle_key top_window modifiers (XK.xk_Pointer_Button_Dflt + button)
    (*x: [[Top_window.handler()]] match event cases *)
    | Xtypes.XTMouseMotion (modifiers,button,x,y) ->
        mouse_x := x;
        mouse_y := y;
        handle_key top_window modifiers (XK.xk_Pointer_Drag_Dflt + button)
    (*x: [[Top_window.handler()]] match event cases *)
    | Xtypes.XTResize (new_width, new_height) ->
        resize_window top_window.window 0 0 new_width (new_height - 1);
        top_window.top_mini_buffers |> List.iter (fun frame -> 
          let window = frame.frm_window in
          window.win_ypos <- new_height - 1;
          window.win_width <- new_width;
          Frame.install window frame
        );
        top_window.top_width <- new_width;
        top_window.top_height <- new_height;
        clear_message top_window;
        update_display ()
    (*e: [[Top_window.handler()]] match event cases *)
  )
(*e: function Top_window.handler *)


(*s: constant Top_window.buffers_menu *)
let buffers_menu = ref 
  (fun (top_window : top_window) (button : (*WX_button.t*) unit) ()
  -> ())
(*e: constant Top_window.buffers_menu *)

(*s: function Top_window.scroll_to_frame *)
(*
let scroll_to_frame ady top_window =
  let frame = top_window.top_active_frame in
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let pos_start = Text.get_position text frame.frm_start in
  let _size = Text.size text in
  Common.pr2 "ady#get_pos size";
  let y = 1
(*    ady#get_pos size  *)
  in
  if abs (y - pos_start) > 80 then
  (* the position has changed *)
    begin
      frame.frm_force_start <- true;
      frame.frm_redraw <- true;
      Text.set_position text frame.frm_start y
    end
*)
(*e: function Top_window.scroll_to_frame *)


(*s: constant Top_window.menus *)
(*
let menus = define_option ["menus"] ""
    (list_option (tuple2_option (string_option, list_option string2_option)))
  []
*)
(*e: constant Top_window.menus *)
(*s: constant Top_window.file_menu *)
let file_menu = define_option ["file_menu"] "" (list_option string2_option) []
(*e: constant Top_window.file_menu *)
(*s: constant Top_window.edit_menu *)
let edit_menu = define_option ["edit_menu"] "" (list_option string2_option) []
(*e: constant Top_window.edit_menu *)
(*s: constant Top_window.help_menu *)
let help_menu = ref ([| |]: (string * action) array)
(*e: constant Top_window.help_menu *)

  
  
(*s: function Top_window.create *)
let create () =
  let loc = Globals.location() in
  let buf = 
    Ebuffer.default "*help*" in
  (* keep one line for the minibuffer, hence the -1 *)
  let window = 
    Window.create_at_top  0 0 loc.loc_width (loc.loc_height - 1) in
  let frame = 
    Frame.create_without_top window None buf in
  let top_window =
    { 
      top_width = loc.loc_width;
      top_height = loc.loc_height;
      window = window;
      top_active_frame = frame;

      top_name = buf.buf_name;

      top_mini_buffers = [];
      top_second_cursor = None;

      graphics = None;
    } 
  in

  (* adjust what Window.create_at_top could not do *)
  frame.frm_window.win_up <- TopWindow top_window;
  loc.top_windows <- top_window :: loc.top_windows;

  top_window
(*e: function Top_window.create *)

(*s: function Top_window.delete_window *)
let delete_window frame =
  failwith "Top_window:delete_window: TODO"
(*
  let top_window = Window.top frame.frm_window in
  let loc = Efuns.loc() in
  if List.length loc.top_windows > 1 then
    let xterm = Window.xterm top_window in
    top_window.top_appli#destroy;
    WX_xterm.destroy_window xterm;
    Frame.kill_all top_window.window;
    loc.top_windows <- Utils.list_remove loc.top_windows
      top_window
*)
(*e: function Top_window.delete_window *)

        
(*s: constant Top_window.check_abort *)
(*let check_abort = ref 0*)
(*e: constant Top_window.check_abort *)
(*s: constant Top_window.check_abort_delay *)
(*let check_abort_delay = ref 100*)
(*e: constant Top_window.check_abort_delay *)

  (* This function should be used in loops that could be interrupted by
  the user. It returns "true" if the user pressed C-g, and false
  in other cases. *)
  
(*s: function Top_window.check_abort *)
let check_abort frame =
  failwith "Top_window.check_abort: TODO"
(*
  incr check_abort; 
  if !check_abort mod !check_abort_delay = 0 then
    let top_window = Window.top frame.frm_window in
    let xterm = top_window.top_term in
    xterm#check_abort
  else false
*)
(*e: function Top_window.check_abort *)

(*e: graphics/top_window.ml *)
