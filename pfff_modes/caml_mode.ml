(* Yoann Padioleau
 *
 * Copyright (C) 2015, 2018 Yoann Padioleau
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
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Pfff-based OCaml major mode.
 * 
 * This module provides a major mode to edit OCaml files by using
 * the OCaml parser and highlighter in Pfff (which is also used for codemap).
 * It relies on ocp-indent and merlin for the rest.
 *
 * todo:
 *  - Tab handling when in comment and indent and add '*' leading if needed
 *  - finish Merlin support (see ocaml_merlin.ml)
 *)

(*****************************************************************************)
(* Pfff specifics *)
(*****************************************************************************)
let funcs = { Pfff_modes.
  parse = (fun file ->
    Common.save_excursion Flag_parsing_ml.error_recovery true (fun()->
      let (astopt, toks), _stat = Parse_ml.parse file in
      [astopt ||| [], toks]
    )
  );
  highlight = (fun ~tag_hook prefs (ast, toks) -> 
    Highlight_ml.visit_program ~tag_hook prefs (ast, toks)
  );
  }

(*****************************************************************************)
(* Colors *)
(*****************************************************************************)

let color_buffer buf =
  let s = Text.to_string buf.buf_text in
  (* we need to keep the extension because Parse_ml.parse behaves
   * differently on ml and mli files
   *)
  let ext =
    match buf.buf_filename with
    | None -> "ml"
    | Some file -> 
        let (_,_, e) = Common2.dbe_of_filename file in
        e
  in
  Common2.with_tmp_file ~str:s ~ext (fun file ->
    Pfff_modes.colorize_and_set_outlines funcs buf file
  )

(*****************************************************************************)
(* Indentation *)
(*****************************************************************************)
(* See ocaml_ocp_indent.ml *)

(*****************************************************************************)
(* Completion/navigation/... *)
(*****************************************************************************)
(* see ocaml_merlin.ml *)

(*****************************************************************************)
(* The mode *)
(*****************************************************************************)
let hooks = Store.create_abstr "caml_mode_hook"

let mode =  Ebuffer.new_major_mode "OCaml(Pfff)" (Some (fun buf ->
  color_buffer buf; 

  buf.buf_syntax_table.(Char.code '_') <- true;
  buf.buf_syntax_table.(Char.code '\'') <- true;
  buf.buf_syntax_table.(Char.code '.') <- false;

  (* alt: activate them in hooks in pad.ml if you dont want them
   * by default for every users or want exotic parenthesis (e.g., |])
   *)
  Minor_modes.toggle_minor_buffer Ocaml_merlin.mode buf;
  Minor_modes.toggle_minor_buffer Paren_mode.mode buf;

  let hooks = Var.get_var buf hooks in
  Hook.exec_hooks hooks buf;
))

let caml_mode = 
  Major_modes.enable_major_mode mode
[@@interactive]

(*****************************************************************************)
(* Setup *)
(*****************************************************************************)

let _ =
  Hook.add_start_hook (fun () ->
    Var.add_global Ebuffer.modes_alist 
      (* the .mll and .mly are better handled by ocaml_mode for now *)
      [".*\\.\\(ml\\|mli\\)$",mode];
    
    (* reuse some functions from ocaml_mode.ml from LeFessant *)
    Var.set_major_var mode Compil.find_error Ocaml_mode.find_error;
    Var.set_major_var mode Compil.find_error_location_regexp 
      (snd !!Ocaml_mode.error_regexp);

    Var.set_major_var mode Indent.indent_func Ocaml_mode.indent_between_points;
    Keymap.add_major_key mode [NormalMap,XK.xk_Tab] 
     Ocaml_mode.indent_current_line;

    (* recolor at save time *)
    Var.set_major_var mode Ebuffer.saved_buffer_hooks
      (color_buffer::(Var.get_global Ebuffer.saved_buffer_hooks));
  )
