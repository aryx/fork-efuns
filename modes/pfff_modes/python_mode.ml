(* Yoann Padioleau
 *
 * Copyright (C) 2019 Yoann Padioleau
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
module PH = Parse_and_highlight

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Pfff-based Python major mode.
 * 
 * This module provides a major mode to edit Python files by using
 * the Python parser and highlighter in Pfff 
 * (which is also used for codemap).
 *
 * todo:
 *  - support Bento
 *)

(*****************************************************************************)
(* Pfff specifics *)
(*****************************************************************************)
(* TODO: factorize with codemap in PH.python_parse_and_highlight *)
let funcs = { PH.
  parse = (fun file ->
    Common.save_excursion Flag_parsing.error_recovery true (fun()->
      let { Parsing_result.ast; tokens; _} = Parse_python.parse file in
      Some ast, tokens
    )
  );
  highlight = (fun ~tag_hook prefs _file (ast, toks) -> 
    Highlight_python.visit_program ~tag_hook prefs (ast, toks)
  );
  info_of_tok = (fun _ -> failwith "not needed");
  }

(*****************************************************************************)
(* Colors *)
(*****************************************************************************)

let color_buffer buf =
  let s = Text.to_string buf.buf_text in
  UTmp.with_temp_file ~contents:s ~suffix:"py" (fun file ->
    Pfff_modes.colorize_and_set_outlines funcs buf file
  )

(*****************************************************************************)
(* Indentation *)
(*****************************************************************************)

(*****************************************************************************)
(* Completion/navigation/... *)
(*****************************************************************************)

(*****************************************************************************)
(* The mode *)
(*****************************************************************************)
let hooks = Store.create_abstr "python_mode_hook"

let mode =  Ebuffer.new_major_mode "Python(Pfff)" (Some (fun buf ->
  color_buffer buf; 

  buf.buf_syntax_table.(Char.code '_') <- true;

  (* alt: activate them in hooks in pad.ml if you dont want them
   * by default for every users or want exotic parenthesis (e.g., |])
   *)
  (* Minor_modes.toggle_minor_on_buf Ocaml_merlin.mode buf; *)
  Minor_modes.toggle_minor_on_buf Paren_mode.mode buf;

  let hooks = Var.get_var buf hooks in
  Hooks.exec_hooks hooks buf;
))

let python_mode = 
  Major_modes.enable_major_mode mode
[@@interactive]

(*****************************************************************************)
(* Setup *)
(*****************************************************************************)

let _ =
  Hooks.add_start_hook (fun () ->
    Var.add_global Ebuffer.modes_alist 
      [".*\\.\\(py\\|py\\)$",mode];
    
    (* recolor at save time *)
    Var.set_major_var mode Ebuffer.saved_buffer_hooks
      (color_buffer::(Var.get_global Ebuffer.saved_buffer_hooks));
    Var.set_global hooks [];
  )
