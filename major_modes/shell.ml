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
(* Prelude *)
(*****************************************************************************)

(* An eshell inspired shell/terminal for efuns.
*)


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)


(*****************************************************************************)
(* Install *)
(*****************************************************************************)

let install buf =
  ()

let mode =  Ebuffer.new_major_mode "Shell" [install]
let shell_mode frame = Ebuffer.set_major_mode frame.frm_buffer mode

let eshell frame =
  let buf = Ebuffer.default "*Shell*" in
  let text = buf.buf_text in
  let loc = Efuns.location () in
  let str = spf "%s $ " loc.loc_dirname in
  Text.update text str;
  Frame.change_buffer frame.frm_window "*Shell*";
  Text.set_position text frame.frm_point (Text.size text);
  shell_mode frame;
  ()


(*****************************************************************************)
(* Setup *)
(*****************************************************************************)

let _ = 
  Efuns.add_start_hook (fun () ->
    Keymap.define_interactive_action "eshell" eshell;
    Keymap.define_interactive_action "shell" eshell;
  )

