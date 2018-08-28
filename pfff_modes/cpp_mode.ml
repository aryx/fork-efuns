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
open Efuns

module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Using the C/cpp/C++ parser and highlighters in pfff (used for codemap)
 * for efuns.
 *)

(*****************************************************************************)
(* Pfff specifics *)
(*****************************************************************************)

let funcs = { Pfff_modes.
  parse = (fun file ->
    let (ast2, _stat) = Parse_cpp.parse file in
    let ast = Parse_cpp.program_of_program2 ast2 in
    (* work by side effect on ast2 too *)
    Check_variables_cpp.check_and_annotate_program ast;
    ast2
  );
  highlight = (fun ~tag_hook prefs (ast, toks) -> 
    Highlight_cpp.visit_toplevel ~tag_hook prefs (ast, toks)
  );
  }

(*****************************************************************************)
(* Colors *)
(*****************************************************************************)

let color_buffer buf =
  let s = Text.to_string buf.buf_text in
  Common2.with_tmp_file ~str:s ~ext:"c" (fun file ->
    Pfff_modes.colorize_and_set_outlines funcs buf file
  )

(*****************************************************************************)
(* Installation *)
(*****************************************************************************)

let mode = Ebuffer.new_major_mode "C" (Some (fun buf ->
  color_buffer buf; 

  let tbl = Ebuffer.create_syntax_table () in
  buf.buf_syntax_table <- tbl;
  tbl.(Char.code '_') <- true;
  ()
))

let cpp_mode frame = 
  Ebuffer.set_major_mode frame.frm_buffer mode

(*****************************************************************************)
(* Setup *)
(*****************************************************************************)

let _ =
  Hook.add_start_hook (fun () ->
    Var.add_global Ebuffer.modes_alist 
      [".*\\.\\(c\\|cpp\\|cc\\|h\\|H\\|C\\|y\\|l\\)$", mode];
    Action.define_action "cpp_mode" cpp_mode;
  )
