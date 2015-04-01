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
open Options

open Efuns
(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A port of my (colorized) buffer-menu from emacs to efuns.
*)

let buflist_name = "*Buffer List*"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let list = ref []

let change_buffer_record frame =
  let name = frame.frm_buffer.buf_name in
  if name = buflist_name
  then ()
  else list :=  name :: Utils.list_removeq !list name;
  ()

let buflist_array = Local.create_abstr "buffer_menu_buflist"

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let install buf =
  ()

let mode =  Ebuffer.new_major_mode "Buffer List" [install]


(* bounded to C-M-Tab in std_efunsrc.ml  *)
let menu frame =
  let buf = Ebuffer.default buflist_name in
  Ebuffer.set_major_mode buf mode;

  let text = buf.buf_text in
  let str = " MR Buffer             Size  Mode         File\n" in
  (* M = Modified, R = Read-only? *)
  Text.update text str;
  Text.insert_at_end text " -- ------             ----  ----         ----\n";

  let current = frame.frm_buffer.buf_name in
  let list = Utils.list_removeq !list current in
  let list =
    if current <> buflist_name
    then current :: list
    else list
  in
  let list =
    list @ 
    (Simple.buffer_list frame |> Common.exclude (fun str ->List.mem str list))
  in
  Efuns.set_local buf buflist_array (Array.of_list list);

  list |> List.iter (fun name ->
    let buf = Ebuffer.default name in
    Text.insert_at_end text 
      (spf " %s  %-17s%6d  %-13s%s\n"
         (if (buf.buf_last_saved = Text.version buf.buf_text) then " " else "*")
         buf.buf_name
         (Text.size buf.buf_text)
         buf.buf_major_mode.maj_name
         (match buf.buf_filename with
         | None -> ""
         | Some s -> s
         ));
  );
  Dircolors.colorize buf;
  (*  Text.toggle_readonly text; *)
  Text.goto_line text buf.buf_point 2;
  Frame.change_buffer frame.frm_window "*Buffer List*";

  ()


let key_return frame =
  let buf = frame.frm_buffer in
  let arr = Efuns.get_local buf buflist_array in
  let point = frame.frm_point in
  let line = Text.point_line buf.buf_text point in
  try 
    (* -2 because of header *)
    let buf_name = arr.(line - 2) in
    let window = frame.frm_window in
    Frame.change_buffer window buf_name
  with exn ->
    Top_window.message 
      (Window.top frame.frm_window)
      (spf "not valid entry in buffer list, exn = %s" (Common.exn_to_s exn))

    
  

(*****************************************************************************)
(* Install *)
(*****************************************************************************)

(*****************************************************************************)
(* Setup *)
(*****************************************************************************)

let _ = 
  Efuns.add_start_hook (fun () ->
    define_action "buffer_menu_change_buffer_record" change_buffer_record;
     Frame.change_buffer_hooks =:=
      ("buffer_menu_change_buffer_record" :: !!Frame.change_buffer_hooks);

    let map = mode.maj_map in
    Keymap.add_binding map [(NormalMap, XK.xk_Return)] key_return;

  )
