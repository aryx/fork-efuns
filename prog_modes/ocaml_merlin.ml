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
open Common
open Efuns
module J = Json_type

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Support for Merlin (https://github.com/ocaml/merlin), an external
 * program providing many services for OCaml programs:
 *     - intellisense completion
 *     - jump-to-definition (similar to Tags, but probably more precise)
 *     - on-the-fly error-checking (a la flymake but more robust probably)
 *     - type under cursor (similar to what the .annot provided under Emacs)
 * 
 * Note that Emacs supports Merlin, but I actually had troubles 
 * to setting it up for my Emacs 23 ... Switching to Efuns might be simpler :)
 *
 * Look at merlin/doc/dev/PROTOCOL.md to see the list of commands.
 * You can also experiment with merlin directly from the command line:
 * 
 *   $ ocamlmerlin single errors -filename foo.ml < foo.ml
 * 
 * Note the -filename option is optional but very important for multi-file
 * support. Indeed, it is this option that allows merlin to process
 * the .merlin in your project.
 * 
 * Some good source of inspiration for the code in this file:
 *  - merlin-acme (https://github.com/raphael-proust/merlin-acme) which is a
 *    plugin written in OCaml and 
 *  - sublime-text-merlin (https://github.com/cynddl/sublime-text-merlin)
 *    which is written in Python and pretty small compared to
 *    the plugins for Emacs or Vim.
 * 
 * related:
 * - ocamlspotter
 * - otags
 * - pfff (lang_ml/ and lang_cmt/)
 * 
 *)

(*****************************************************************************)
(* Constants and globals *)
(*****************************************************************************)
let external_program = "ocamlmerlin"

let debug = ref true

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let str_of_current_position frm =
  let point = frm.frm_point in
  let text = frm.frm_buffer.buf_text in
  spf "'%d:%d'" 
    (Text.point_line text point + 1)
    (Text.point_col text point)

(*****************************************************************************)
(* External program communication *)
(*****************************************************************************)
let send_command frm command =
  let buf = frm.frm_buffer in
  let text = buf.buf_text in
  let final_command = 
    spf "%s single %s %s" external_program command
      (match frm.frm_buffer.buf_filename with
      | None -> ""
      | Some file -> spf "-filename '%s'" file
      )
  in
  let (pipe_read, pipe_write) = Unix.open_process final_command in
  let content = Text.to_string text in
  output_string pipe_write content;
  close_out pipe_write;
  let str = input_line pipe_read in
  let _status = Unix.close_process (pipe_read, pipe_write) in
  if !debug
  then pr2 str;
  let j = Json_io.json_of_string str in
  (match j with
  | J.Object (
    ("class", J.String "return")::
    ("value", v)::
      _rest
  ) -> v, str
  | _ -> failwith (spf "wrong ocamlmerlin JSON output: %s" str)
  )

    
  

(*****************************************************************************)
(* Services *)
(*****************************************************************************)

let type_at_cursor frm =
  let command = spf "type-enclosing -position %s -index 0" 
    (str_of_current_position frm) in
  let (j, str) = send_command frm command in
  match j with
  | J.Array (J.Object [
    "start", _;
    "end", _;
    "type", J.String str;
    "tail", _;
  ]::_) -> 
    Top_window.message (Window.top frm.frm_window) str
  | _ -> failwith (spf "wrong JSON output for type_at_cursor: %s" str)


(*****************************************************************************)
(* Minor mode *)
(*****************************************************************************)

let mode = Ebuffer.new_minor_mode  "Merlin" [(fun buf ->
  ()
)]

(*****************************************************************************)
(* Setup *)
(*****************************************************************************)

let _ = 
  Action.define_action "merlin_mode" (Minor_modes.toggle_minor mode);
  Action.define_action "merlin_type" type_at_cursor;
  Keymap.add_binding mode.min_map [Keymap.c_c;ControlMap, Char.code 't']
    type_at_cursor;
  ()
