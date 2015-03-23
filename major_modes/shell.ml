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

let pwd_var = Local.create_string ""

let pwd buf =
  Efuns.get_local buf pwd_var
  
  

let prompt buf =
  spf "%s $ " (pwd buf)

let display_prompt buf =
  Text.insert_at_end buf.buf_text (prompt buf)

(*****************************************************************************)
(* Builtins *)
(*****************************************************************************)

let builtin_ls buf =
  let dir = pwd buf in
  let files = Utils.file_list dir in

  (* similar to Select.complete_filename *)
  let files = files |> List.map (fun file ->
    let path = Filename.concat dir file in
    try 
      let stat = Unix.stat path in
      match stat.Unix.st_kind with
      | Unix.S_DIR -> file ^ "/"
      | _ -> file
    with exn -> 
      pr2 (spf "builtin_ls: exn = %s" (Common.exn_to_s exn));
      file
  )
  in

  (* similar to Select.display_completions *)
  let rec iter list s =
    match list with
    | [] -> s
    | [f] -> Printf.sprintf "%s\n%s" s f
    | f1::f2::tail  ->
      iter tail (Printf.sprintf "%s\n%-40s%s" s f1 f2)
  in
  Text.insert_at_end buf.buf_text "\n";
  Text.insert_at_end buf.buf_text (iter files "");
  Text.insert_at_end buf.buf_text "\n";
  display_prompt buf

let builtin_cd buf s =
  Efuns.set_local buf pwd_var s;
  builtin_ls buf

(*****************************************************************************)
(* Colors *)
(*****************************************************************************)

let prompt_color = "coral"

let colorize buf =
  Dircolors.colorize buf;
  Simple.color buf 
    (Str.regexp ("^/.* \\$")) false
      (Text.make_attr (Window.get_color prompt_color) 1 0 false);
  ()
  

(*****************************************************************************)
(* Interpreter *)
(*****************************************************************************)
let interpret frame s =
  let buf = frame.frm_buffer in
  (match s with
  | "ls" -> builtin_ls buf
  | _ when s =~ "cd[ ]+\\(.*\\)" -> builtin_cd buf (Common.matched1 s)
  | cmd ->
      raise Todo
  );
  colorize buf
  


(*****************************************************************************)
(* Install *)
(*****************************************************************************)

let install buf =
  Efuns.set_local buf pwd_var 
  (* todo: use the dirname of the file in current frame 
     Frame.current_dir?
  *)
    (Efuns.location()).loc_dirname;
  (* !!! *)
  buf.buf_sync <- true;
  display_prompt buf;
  colorize buf;
  ()

let mode =  Ebuffer.new_major_mode "Shell" [install]
let shell_mode frame = Ebuffer.set_major_mode frame.frm_buffer mode

let eshell frame =
  let buf_name = "*Shell*" in
  let text = Text.create "" in
  let buf = Ebuffer.create buf_name None text (Keymap.create ()) in
  Ebuffer.set_major_mode buf mode;
  Frame.change_buffer frame.frm_window buf.buf_name;
  ()

let key_return frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  Text.with_dup_point text frame.frm_point (fun point ->
    let delta = Text.search_backward text (Str.regexp " \\$ ") point in
    Text.fmove text point delta;
    let s = Text.region text frame.frm_point point in
    interpret frame s
  )


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

