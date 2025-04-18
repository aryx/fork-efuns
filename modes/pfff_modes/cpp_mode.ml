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
module PH = Parse_and_highlight

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Pfff-based C/C++ major mode.
 *
 * This mode uses the C/cpp/C++ parser and highlighters in Pfff 
 * (used for codemap) for Efuns.
 *)

(*****************************************************************************)
(* Pfff specifics *)
(*****************************************************************************)

(* TODO: factorize with codemap in a PH.cpp_parse_and_highlight *)
let funcs = { PH.
  parse = (fun file ->
    Common.save_excursion Flag_parsing.error_recovery true (fun()->
      let { Parsing_result. ast; tokens; _ } = Parse_cpp.parse file in
      (* work by side effect on ast2 too *)
      (* TODO in trimmed pfff
         Check_variables_cpp.check_and_annotate_program ast;
       *)
      ast, tokens
    )
  );
  highlight = (fun ~tag_hook prefs file (ast, toks) -> 
    Highlight_cpp.visit_toplevel ~tag_hook prefs file (ast, toks)
  );
  info_of_tok = (fun _ -> failwith "not needed");
  }

(*****************************************************************************)
(* Colors *)
(*****************************************************************************)

let color_buffer buf =
  let s = Text.to_string buf.buf_text in
  UTmp.with_temp_file ~contents:s ~suffix:"c" (fun file ->
    Pfff_modes.colorize_and_set_outlines funcs buf file
  )

(*****************************************************************************)
(* The mode *)
(*****************************************************************************)
let hooks = Store.create_abstr "cpp_mode_hook"

let mode = Ebuffer.new_major_mode "Cpp(Pfff)" (Some (fun buf ->
  color_buffer buf; 

  buf.buf_syntax_table.(Char.code '_') <- true;

  Minor_modes.toggle_minor_on_buf Paren_mode.mode buf;

  let hooks = Var.get_var buf hooks in
  Hooks.exec_hooks hooks buf;
))

let cpp_mode = 
  Major_modes.enable_major_mode mode
[@@interactive]

(*****************************************************************************)
(* Setup *)
(*****************************************************************************)

let _ =
  Hooks.add_start_hook (fun () ->
    (* this should override the one in c_mode.ml because this module is
     * linked after and Var.add_global prepends.
     *)
    Var.add_global Ebuffer.modes_alist 
      [".*\\.\\(c\\|cpp\\|cc\\|h\\|H\\|C\\|y\\|l\\)$", mode];

    (* reuse indentation functions in c_mode.ml *)
    Var.set_major_var mode Indent.indent_func C_mode.indent_between_points;
    Keymap.add_major_key mode [NormalMap,XK.xk_Tab] C_mode.indent_current_line;

    (* recolor at save time *)
    Var.set_major_var mode Ebuffer.saved_buffer_hooks
      (color_buffer::(Var.get_global Ebuffer.saved_buffer_hooks));
    Var.set_global hooks [];
  )
