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
open Options

module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Using the TeX/Noweb parser and highlighters in pfff (used for codemap)
 * for efuns.
 *
 * todo: look at tex_mode.mll? some good ideas? useful keys?
 *)

(*****************************************************************************)
(* Pfff specifics *)
(*****************************************************************************)

let funcs = { Pfff_modes.
  parse = (fun file ->
    let (ast2, _stat) = Parse_nw.parse file in
    [ast2]
  );
  highlight = (fun ~tag_hook prefs (ast, toks) -> 
    Highlight_nw.visit_program ~tag_hook prefs (ast, toks)
  );
  }

(*****************************************************************************)
(* Colors *)
(*****************************************************************************)

let color_buffer buf =
  let s = Text.to_string buf.buf_text in
  Common2.with_tmp_file ~str:s ~ext:"c" (fun file ->
    Pfff_modes.colorize_and_set_outlines funcs buf file
  );
  Simple.color buf (Str.regexp "^%.*$") false
    (Text.make_attr (Attr.get_color !!Pl_colors.syncweb_comment_color) 1 0 false);
  Simple.color buf (Str.regexp "^%[a-zA-Z]+:") false
    (Text.make_attr (Attr.get_color !!Pl_colors.section_comment_color) 1 0 false);
  ()


(*****************************************************************************)
(* Installation *)
(*****************************************************************************)

let mode = Ebuffer.new_major_mode "Noweb" (Some (fun buf ->
  color_buffer buf; 

  let tbl = Ebuffer.create_syntax_table () in
  buf.buf_syntax_table <- tbl;
  (* true in code usually, not necesseraly for tex itself, but have
   * to pick one
   *)
  tbl.(Char.code '_') <- true;
  ()
))

let noweb_mode frame = 
  Ebuffer.set_major_mode frame.frm_buffer mode

(*****************************************************************************)
(* Setup *)
(*****************************************************************************)

let _ =
  Hook.add_start_hook (fun () ->
    Var.add_global Ebuffer.modes_alist [".*\\.nw$", mode];
    Action.define_action "noweb_mode" noweb_mode;
  )
