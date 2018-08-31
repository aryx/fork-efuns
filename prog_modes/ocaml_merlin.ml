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
(* Support for Merlin (https://github.com/ocaml/merlin) in Efuns.
 *
 * Merlin is an external program providing many services for OCaml programs:
 *   - intellisense completion,
 *     which is smarter than dabbrev
 *   - jump-to-definition,
 *     which is similar to Tags, but is easier to setup, more incremental,
 *     more precise, and faster
 *   - on-the-fly error-checking,
 *     which is similar to flymake, but again easier to setup
 *   - type under cursor, 
 *     which is similar to what the .annot provided under Emacs, but is
 *     incremental by not requiring to refresh the .annot by typing 'make'
 *     
 * 
 * To use merlin from Efuns simply 'opam install merlin', which
 * should make available an 'ocamlmerlin' program in your PATH.
 * 
 * Note that Emacs supports Merlin, but I actually had troubles 
 * to setting it up for my Emacs 23 ... Switching to Efuns might be simpler :)
 *
 * internals: 
 * Look at merlin/doc/dev/PROTOCOL.md to see the list of commands.
 * You can also experiment with merlin directly from the command line:
 * 
 *   $ ocamlmerlin single errors -filename foo.ml < foo.ml
 * 
 * Note that the '-filename' option is optional but very important for
 * multi-files support. Indeed, it is this option that allows merlin to find
 * and process the .merlin in your project.
 * 
 * Some source of inspiration for the code in this file:
 *  - merlin-acme (https://github.com/raphael-proust/merlin-acme) which is a
 *    plugin written in OCaml
 *  - sublime-text-merlin (https://github.com/cynddl/sublime-text-merlin)
 *    which is written in Python
 * The code of those plugins is small compared to the plugins for Emacs or Vim.
 * 
 * FAQ:
 *  - How can I jump to the definition of entities in OPAM packages
 *    when those packages do not have the .cmt or .cmti files?
 * 
 *    See https://github.com/ocaml/merlin/wiki/Letting-merlin-locate-go-to-stuff-in-.opam and put
 *    $ export OPAMKEEPBUILDDIR=true
 *    $ export OCAMLPARAM="_,bin-annot=1"
 *    in your environment before installing OPAM packages.
 *    Then add a symlink from external/opam_lib/build to ../build
 *    and add this to your .merlin:
 *     B external/opam_lib/build/**
 *     S external/opam_lib/build/**
 * 
 * 
 * related:
 * - otags
 * - ocamlspotter
 * - pfff (lang_ml/ and lang_cmt/)
 * - https://github.com/freebroccolo/ocaml-language-server but is written
 *   in Javascript and uses merlin under the hood
 * 
 * todo:
 *  - jump
 *  - case-analysis
 *  - errors
 * less:
 *  - occurences
 *  - type-enclosing, type-expression
 *  - outline, shape
 *  - expand-prefix (more error resistant than complete-prefix,
 *    maybe use if complete-prefix returns an empty set)
 *  - enclosing
 *  - extension-list, findlib-list, flags-list
 *  - phrase
 *  - list-modules
 *  - path-of-source
 *  - check-configuration
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
let execute_command frm command =
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
    ("class", J.String "return"):: (* | "failure" | "error" | "exception" *)
    ("value", v)::
    ("notifications", J.Array [])::
    ("timing", _)::
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
  let (j, str) = execute_command frm command in
  match j with
  | J.Array (J.Object [
    "start", _;
    "end", _;
    "type", J.String str;
    "tail", _;
  ]::_) -> 
    Top_window.message (Window.top frm.frm_window) str
  | _ -> failwith (spf "wrong JSON output for type_at_cursor: %s" str)

let doc_at_cursor frm =
  let command = spf "document -position %s" 
    (str_of_current_position frm) in
  let (j, str) = execute_command frm command in
  match j with
  | J.String str -> 
    Top_window.message (Window.top frm.frm_window) str
  | _ -> failwith (spf "wrong JSON output for doc_at_cursor: %s" str)

let def_at_cursor look_for frm =
  let command = spf "locate -position %s -look-for %s" 
    (str_of_current_position frm) look_for in
  let (j, str) = execute_command frm command in
  match j with
  | J.Object [
    "file", J.String file;
    "pos", J.Object ["line", J.Int line; "col", J.Int col];
  ] -> 
    let str = spf "file: %s (%d:%d)" file line col in
    (* for C-M-l to work *)
    Multi_buffers.set_previous_frame frm;

    let frm = Frame.load_file frm.frm_window file in
    Text.goto_line frm.frm_buffer.buf_text frm.frm_point (line - 1);
    Text.fmove frm.frm_buffer.buf_text frm.frm_point (col);
    Top_window.message (Window.top frm.frm_window) str

  | J.String str -> 
    Top_window.message (Window.top frm.frm_window) str
  | _ -> failwith (spf "wrong JSON output for def_at_cursor: %s" str)
  


let complete_prefix frm =
  raise Todo

(*****************************************************************************)
(* Minor mode *)
(*****************************************************************************)

let mode = Ebuffer.new_minor_mode  "Merlin" []

(*****************************************************************************)
(* Setup *)
(*****************************************************************************)

let _ = 
  Action.define_action "merlin_mode" (Minor_modes.toggle_minor mode);

  Keymap.add_binding mode.min_map [Keymap.c_c;ControlMap, Char.code 't']
    type_at_cursor;
  (* 'i' for information *)
  Keymap.add_binding mode.min_map [Keymap.c_c;ControlMap, Char.code 'i']
    doc_at_cursor;
  (* replacement for the traditional Emacs tags-find command *)
  Keymap.add_binding mode.min_map [MetaMap, Char.code '.']
    (def_at_cursor "implementation");
  (* 'd' for def *)
  Keymap.add_binding mode.min_map [Keymap.c_c;ControlMap, Char.code 'd']
    (def_at_cursor "implementation");
  Keymap.add_binding mode.min_map [Keymap.c_c;ControlMap, Char.code 'D']
    (def_at_cursor "interface");
  ()
