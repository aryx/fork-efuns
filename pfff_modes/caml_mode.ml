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
 * Using the OCaml parser and highlighters in pfff (used for codemap)
 * for efuns.
 *)

(*****************************************************************************)
(* Colors *)
(*****************************************************************************)

let colorize buf file =
  let (astopt,toks), _stat = 
    Common.save_excursion Flag_parsing_ml.error_recovery true (fun()->
      Parse_ml.parse file 
    )
  in
  let prefs = Highlight_code.default_highlighter_preferences in

  let text = buf.buf_text in
  let cursor = Text.new_point text in

  let ast = astopt ||| [] in
  (ast, toks) |> Highlight_ml.visit_program ~tag_hook:(fun info categ ->
    let color = Pfff_modes.color_of_categ categ in
    let fontsize = Pfff_modes.size_of_categ categ in

    let pos = PI.pos_of_info info in
    Text.set_position text cursor pos;
    let attr = Text.make_attr (Window.get_color color) 1 fontsize false in

    let str = PI.str_of_info info in
    let len = String.length str in
    Text.set_attr text cursor len attr
  ) prefs 
  
  

let caml_color_region buf start_point end_point =
  raise Todo

let caml_color_buffer buf =
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
    colorize buf file
  )


(*****************************************************************************)
(* Installation *)
(*****************************************************************************)

let install buf =
  caml_color_buffer buf; 
  buf.buf_syntax_table.(Char.code '_') <- true;
  ()


let mode =  Ebuffer.new_major_mode "Caml" [install]
let caml_mode frame = Ebuffer.set_major_mode frame.frm_buffer mode


(*****************************************************************************)
(* Setup *)
(*****************************************************************************)

let setup () = 
  define_action "caml_mode" caml_mode;
  define_action "caml_mode.color_buffer" (fun frame -> 
    caml_color_buffer frame.frm_buffer
  );
  ()

let mode_regexp =
  [".*\\.\\(ml\\|mli\\|mll\\|mly\\)"]

let _ =
  Efuns.add_start_hook (fun () ->
    let alist = get_global Ebuffer.modes_alist in
    set_global Ebuffer.modes_alist 
      ((List.map (fun s -> s, mode) mode_regexp) @ alist);
    
    setup();
  )
