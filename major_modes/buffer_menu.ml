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
(* A port of my (colorized) buffer-menu from emacs to efuns.
*)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let menu frame =
  let buf = Ebuffer.default "*Buffer List*" in
  let text = buf.buf_text in
  let str = " MR Buffer           Size  Mode         File\n" in
  Text.update text str;
  Text.insert_at_end text " -- ------           ----  ----         ----\n";

  Simple.buffer_list frame |> List.iter (fun name ->
    let buf = Ebuffer.default name in
    Text.insert_at_end text 
      (spf " %s  %-17s%4d  %-14s%s\n"
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
  Frame.change_buffer frame.frm_window "*Buffer List*";
  ()


(*****************************************************************************)
(* Install *)
(*****************************************************************************)

let _ = 
  Efuns.add_start_hook (fun () ->
    ()
  )
