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
(* Helpers *)
(*****************************************************************************)

let prompt () =
  (* todo: use the dirname of the file in current frame 
     Frame.current_dir?
  *)
  let pwd = (Efuns.location ()).loc_dirname in
  spf "%s $ " pwd


(*****************************************************************************)
(* Install *)
(*****************************************************************************)

let install buf =
  ()

let mode =  Ebuffer.new_major_mode "Shell" [install]
let shell_mode frame = Ebuffer.set_major_mode frame.frm_buffer mode

let eshell frame =
  let buf_name = "*Shell*" in
  let text = Text.create "" in
  let cursor = Text.new_point text in
  let buf = Ebuffer.create buf_name None text (Keymap.create ()) in
  (* !!! *)
  buf.buf_sync <- true;

  let str = prompt () in
  Text.insert text cursor str;
  buf.buf_modified <- buf.buf_modified +1;
  Ebuffer.set_major_mode buf mode;
  Frame.change_buffer frame.frm_window buf.buf_name;
  ()

let key_return frame =
  pr2 "RET"


(*****************************************************************************)
(* Setup *)
(*****************************************************************************)

let _ = 
  Efuns.add_start_hook (fun () ->
    Keymap.define_interactive_action "eshell" eshell;
    Keymap.define_interactive_action "shell" eshell;

    Keymap.add_major_key mode [(NormalMap, XK.xk_Return)]
      "key_return" key_return;
  )

