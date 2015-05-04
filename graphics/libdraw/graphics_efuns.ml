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
(* Draw API *)
(*****************************************************************************)
open Xdraw (* for the fields *)

let clear_eol col line len =
  if !Globals.debug_graphics
  then pr2 (spf "clear_eol: %d %d %d" col line len);
  Draw.clear_eol col line len

let draw_string col line  str  offset len   attr =
  if !Globals.debug_graphics
  then pr2 (spf "draw_string %d %d %s %d %d %d"
         col line str offset len attr);

  Draw.clear_eol col line len;
  Draw.draw_string col line (String.sub str offset len);
  ()

let update_display () =
  if !Globals.debug_graphics
  then pr2 ("update_displays")

let backend = { 
  clear_eol = clear_eol; 
  draw_string = draw_string; 
  update_display = update_display;
  update_window_title = (fun _ -> ());
}

(*****************************************************************************)
(* paint/configure/expose *)
(*****************************************************************************)

(*****************************************************************************)
(* The main UI *)
(*****************************************************************************)

let init init_files = 

  (*-------------------------------------------------------------------*)
  (* Graphics initialisation *)
  (*-------------------------------------------------------------------*)
  Draw.initdraw None "test_draw_ml";

  (*-------------------------------------------------------------------*)
  (* Window creation *)
  (*-------------------------------------------------------------------*)

  (*-------------------------------------------------------------------*)
  (* Creation of core DS of Efuns (buffers, frames, top_window) *)
  (*-------------------------------------------------------------------*)

  (* loc.loc_height <- 45; *)
  (* will boostrap and use a newly created *help* buffer *)
  let top_window = Top_window.create () in
  (* the *bindings* buffer *)
  Interactive.create_bindings () |> ignore;
  (* open the first buffers *)
  init_files |> List.iter (fun name ->
    Frame.load_file top_window.window name |> ignore
  );

  (*-------------------------------------------------------------------*)
  (* Layout *)
  (*-------------------------------------------------------------------*)

    (*-------------------------------------------------------------------*)
    (* main view *)
    (*-------------------------------------------------------------------*)

  top_window.graphics <- Some backend;
  Top_window.update_display ();

  (*-------------------------------------------------------------------*)
  (* Events *)
  (*-------------------------------------------------------------------*)

  let escape = ref false in

  (* Main loop *)
  while true do
    let code = Draw.ekbd () in
    (*Printf.printf "key = %d" code;*)

    let modifiers, code = 
      match code with
      (* control-j *)
      | 10 -> 0, XK.xk_Return
      | 8 | 9 | 13  -> 0, code
      | _ when code >= 1 && code <= 26 -> 
          Xtypes.controlMask, code - 1 + Char.code 'a'
      (* escape key *)
      | 27 -> 0, 27

      (* see keyboard.h *)
      | 63488 -> 0, XK.xk_Down
      | 61454 -> 0, XK.xk_Up
      | 61457 -> 0, XK.xk_Left
      | 61458 -> 0, XK.xk_Right

      | _ -> 0, code
    in
    if code = 27
    then escape := true
    else begin
      let modifiers = modifiers lor (if !escape then Xtypes.mod1Mask else 0) in
      escape := false;
      let evt = Xtypes.XTKeyPress (modifiers, spf "%d" code, code) in
      Top_window.handler top_window evt
    end
  done;
  ()

  (*-------------------------------------------------------------------*)
  (* End *)
  (*-------------------------------------------------------------------*)
