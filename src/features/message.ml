(* Yoann Padioleau
 *
 * Copyright (C) 2018 Yoann Padioleau
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
open Efuns

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A better Top_window.message 
 *  - works with a frame; no need to build a top_window from a frame
 *  - put also the messages in *Messages* as in Emacs
 *)

let bufname = "*Messages*"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let add_to_messages s =
  let buf = Ebuffer.default bufname in
  let text = buf.buf_text in
  Text.insert_at_end text (s ^ "\n")

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* note: I could get rid of the frame parameter and compute top_window
 * by getting List.hd (Globals.editors()).top_windows, but maybe
 * one day I will add back the possibility to have multiple top_windows,
 * in which case we will need to know to which top_window to display
 * a message.
 *)
let message frame s =
  let top_window = 
    Window.top frame.frm_window 
  in
  Top_window.message top_window s;
  add_to_messages s
