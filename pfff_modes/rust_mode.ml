(* Yoann Padioleau
 *
 * Copyright (C) 2023 r2c
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
(* Tree-sitter-based Rust major mode.
 * 
 * This module provides a major mode to edit Rust files by using
 * the Rust parser and highlighter in respectively Semgrep and Codemap.
 *
 * todo:
 *  - indentation? LSP server?
 *)

(*****************************************************************************)
(* Colors *)
(*****************************************************************************)

let color_buffer buf =
  let s = Text.to_string buf.buf_text in
  Common2.with_tmp_file ~str:s ~ext:"rs" (fun file ->
    Pfff_modes.colorize_and_set_outlines Parse_and_highlight.rust buf file
  )

(*****************************************************************************)
(* Indentation *)
(*****************************************************************************)
(* *)

(*****************************************************************************)
(* Completion/navigation/... *)
(*****************************************************************************)
(* *)

(*****************************************************************************)
(* The mode *)
(*****************************************************************************)
let hooks = Store.create_abstr "rust_mode_hook"

let mode =  Ebuffer.new_major_mode "Rust(Semgrep)" (Some (fun buf ->
  color_buffer buf; 

  (*
  buf.buf_syntax_table.(Char.code '_') <- true;
  buf.buf_syntax_table.(Char.code '\'') <- true;
  buf.buf_syntax_table.(Char.code '.') <- false;
   *)

  (* alt: activate them in hooks in pad.ml if you dont want them
   * by default for every users or want exotic parenthesis (e.g., |])
   *)
  (* Minor_modes.toggle_minor_on_buf Ocaml_merlin.mode buf; *)
  Minor_modes.toggle_minor_on_buf Paren_mode.mode buf;

  let hooks = Var.get_var buf hooks in
  Hook.exec_hooks hooks buf;
))

let rust_mode = 
  Major_modes.enable_major_mode mode
[@@interactive]

(*****************************************************************************)
(* Setup *)
(*****************************************************************************)

let _ =
  Hook.add_start_hook (fun () ->
    Var.add_global Ebuffer.modes_alist 
      (* the .mll and .mly are better handled by ocaml_mode for now *)
      [".*\\.rs$",mode];

(*    
    (* reuse some functions from ocaml_mode.ml from LeFessant *)
    Var.set_major_var mode Compil.find_error Ocaml_mode.find_error;
    Var.set_major_var mode Compil.find_error_location_regexp 
      (snd !!Ocaml_mode.error_regexp);

    Var.set_major_var mode Indent.indent_func Ocaml_mode.indent_between_points;
    Keymap.add_major_key mode [NormalMap,XK.xk_Tab] 
     Ocaml_mode.indent_current_line;
 *)
    
    (* recolor at save time *)
    Var.set_major_var mode Ebuffer.saved_buffer_hooks
      (color_buffer::(Var.get_global Ebuffer.saved_buffer_hooks));
    Var.set_global hooks [];
  )

