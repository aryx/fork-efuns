(* Yoann Padioleau
 * 
 * Copyright (C) 2015 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

open Efuns

(*****************************************************************************)
(* Final view rendering *)
(*****************************************************************************)

(*****************************************************************************)
(* Draw API *)
(*****************************************************************************)

(* helper *)
let move_to col line =
  pr2 "TODO"
(*
  let (w,h) = Graphics.text_size "d" in
  let size_y = Graphics.size_y () in
  Graphics.moveto (w * col) (size_y - h - (line * h))
*)  

let clear_eol () col line len =
  pr2 "TODO"
(*
  pr2 (spf "WX_xterm.clear_eol: %d %d %d" col line len);
  move_to col line;
  let (w,h) = Graphics.text_size "d" in
  Graphics.set_color Graphics.white;
  Graphics.fill_rect (Graphics.current_x()) (Graphics.current_y())
    (w * len) h;
  ()
*)

let draw_string () col line  str  offset len   attr =
  pr2 "TODO"
(*
  pr2 (spf "WX_xterm.draw_string %d %d %s %d %d %d"
         col line str offset len attr);
  let (w,h) = Graphics.text_size "d" in
  move_to col line;
  Graphics.set_color Graphics.white;
  Graphics.fill_rect (Graphics.current_x()) (Graphics.current_y())
    (w * len) h;
  Graphics.set_color Graphics.black;
  move_to col line;
  Graphics.draw_string (String.sub str offset len);
  ()
*)

let update_displays () =
  pr2 ("WX_xterm.update_displays")


(*****************************************************************************)
(* The main UI *)
(*****************************************************************************)

let init location displayname =
  let _locale = GtkMain.Main.init () in

  (* compute font_size and adjust size of window, or reverse
   * by setting size of font depending on size of window ?
   *)
(*
  let (h, w) = Graphics.textsize "aqd" in
  h := 
*)


  let win = GWindow.window
    ~title:"Efuns"
    ~width:600 ~height:600
    ~allow_shrink:true ~allow_grow:true
    ()
  in

  let quit () = 
    (*Controller.before_quit_all model;*)
    GMain.Main.quit ();
  in

  (*-------------------------------------------------------------------*)
  (* Creation of core DS of Efuns (buffers, frames, top_window) *)
  (*-------------------------------------------------------------------*)

  let display = "" in
  let top_window = Top_window.create location display in

(*
  WX_xterm.setHighlight display 2;
  Dyneval.init true;
  Eval.load top_window "Efunsrc";
  Efuns.init location; (* launch second hooks *)
*)

  let _ = Interactive.create_bindings location in

  (* open the first buffers *)
  !init_files +> List.iter (fun name ->
    let _ = Frame.load_file top_window.window name in ()
  );
  !init_frames +> List.iter (fun str -> 
    let top_window = Top_window.create top_window.top_location
      (Window.display top_window) 
    in
    let _ = Frame.load_file top_window.window str in ()
  );

  Top_window.update_display location;

(*  
  if not (Sys.file_exists (Filename.concat Utils.homedir ".efunsrc")) then
    begin
      Printf.printf "Saving .efunsrc after install"; print_newline ();
      Options.save ();
    end;
*)

(*  if !check then exit 0;   *)


  (*-------------------------------------------------------------------*)
  (* Layout *)
  (*-------------------------------------------------------------------*)

  let vbox = GPack.vbox ~packing:win#add () in

    (*-------------------------------------------------------------------*)
    (* Menu *)
    (*-------------------------------------------------------------------*)


    (*-------------------------------------------------------------------*)
    (* main view *)
    (*-------------------------------------------------------------------*)

    let da = GMisc.drawing_area () in
    da#misc#set_double_buffered false;

    vbox#pack da#coerce;
    da#misc#set_can_focus true ;
    da#event#add [ `KEY_PRESS;
                   `BUTTON_MOTION; `POINTER_MOTION;
                   `BUTTON_PRESS; `BUTTON_RELEASE ];

(*
    da#event#connect#expose ~callback:(expose da w) +> ignore;
    da#event#connect#configure ~callback:(configure da w) +> ignore;

    da#event#connect#button_press   
      (View_matrix.button_action da w) +> ignore;
    da#event#connect#button_release 
      (View_matrix.button_action da w) +> ignore;

    da#event#connect#motion_notify  
      (View_overlays.motion_notify da w) +> ignore; 

*)

  (*-------------------------------------------------------------------*)
  (* End *)
  (*-------------------------------------------------------------------*)

  win#event#connect#delete    ~callback:(fun _  -> quit(); true) +> ignore;
  win#connect#destroy         ~callback:(fun () -> quit(); ) +> ignore;
  win#show ();

  GtkThread.main ();


  (* Main loop *)
  let rec loop () =
    ()
(*
    try
      WX_types.loop ()
    with
      SigInt -> loop ()
*)
(*
    Graphics.loop_at_exit [
      Graphics.Button_down;
      Graphics.Key_pressed;
    ] (fun status ->
      if status.Graphics.keypressed
      then 
        let charkey = status.Graphics.key in
        let code = Char.code charkey in
        pr2 (spf "key: %c, %d" charkey code);
        let modifiers, code = 
          match code with
          | 8 | 9 | 13  -> 0, code
          | _ when code >= 1 && code <= 26 -> 
            Xtypes.controlMask, code - 1 + Char.code 'a'
          | _ -> 0, code
        in
        let evt = Xtypes.XTKeyPress (modifiers, spf "%c" charkey, code) in
        Top_window.handler top_window () evt
    )
*)
  in
  loop ()
