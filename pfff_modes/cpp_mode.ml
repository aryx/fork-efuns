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

module PI = Parse_info
(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Using the C/cpp/C++ parser and highlighters in pfff (used for codemap)
 * for efuns.
 *)

(*****************************************************************************)
(* Colors *)
(*****************************************************************************)

(* dupe of pfff/code_map/draw_microlevel.ml *)
let color_of_categ categ =
  let attrs = Highlight_code.info_of_category categ in
  attrs +> Common.find_some (fun attr ->
    match attr with
    | `FOREGROUND s 
    | `BACKGROUND s (* todo: should really draw the background of the text *)
      -> 
        Some (s)
    | _ -> None
  )

let colorize buf file =

  let (ast2, _stat) = Parse_cpp.parse file in
  let ast = Parse_cpp.program_of_program2 ast2 in
  (* work by side effect on ast2 too *)
  Check_variables_cpp.check_and_annotate_program ast;

  let prefs = Highlight_code.default_highlighter_preferences in

  let text = buf.buf_text in
  let cursor = Text.new_point text in

  ast2 |> List.iter (Highlight_cpp.visit_toplevel ~tag_hook:(fun info categ ->
    let color = color_of_categ categ in

    let pos = PI.pos_of_info info in
    Text.set_position text cursor pos;
    let attr = Text.make_attr (Window.get_color color) 1 0 false in
    let str = PI.str_of_info info in
    let len = String.length str in
    Text.set_attr text cursor len attr
  ) prefs )
  
  

let cpp_color_region buf start_point end_point =
  raise Todo

let cpp_color_buffer buf =
  let s = Text.to_string buf.buf_text in
  Common2.with_tmp_file ~str:s ~ext:"c" (fun file ->
    colorize buf file
  )


(*****************************************************************************)
(* Installation *)
(*****************************************************************************)

let install buf =
  cpp_color_buffer buf; 
  buf.buf_syntax_table.(Char.code '_') <- true;
  ()


let mode =  Ebuffer.new_major_mode "C" [install]
let cpp_mode frame = Ebuffer.set_major_mode frame.frm_buffer mode


(*****************************************************************************)
(* Setup *)
(*****************************************************************************)

let setup () = 
  define_action "cpp_mode" cpp_mode;
  define_action "cpp_mode.color_buffer" (fun frame -> 
    cpp_color_buffer frame.frm_buffer
  );
  ()

let mode_regexp =
  [".*\\.\\(c\\|cpp\\|cc\\|h\\|H\\|C\\|y\\|l\\)$"]

let _ =
  Efuns.add_start_hook (fun () ->
    let alist = get_global Ebuffer.modes_alist in
    set_global Ebuffer.modes_alist 
      ((List.map (fun s -> s, mode) mode_regexp) @ alist);
    
    setup();
  )