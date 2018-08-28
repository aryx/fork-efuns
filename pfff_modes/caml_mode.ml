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
open Options

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
(* Installation *)
(*****************************************************************************)

let mode =  Ebuffer.new_major_mode "OCaml" (Some (fun buf ->
  color_buffer buf; 

  let tbl = Ebuffer.create_syntax_table () in
  buf.buf_syntax_table <- tbl;
  tbl.(Char.code '_') <- true;
  tbl.(Char.code '\'') <- true;
  tbl.(Char.code '.') <- false;
  ()
))

let caml_mode frame = 
  Ebuffer.set_major_mode frame.frm_buffer mode

(*****************************************************************************)
(* Setup *)
(*****************************************************************************)

let _ =
  Hook.add_start_hook (fun () ->
    Var.add_global Ebuffer.modes_alist [".*\\.\\(ml\\|mli\\|mll\\|mly\\)",mode];
    
    Action.define_action "caml_mode" caml_mode;
    Var.set_major_var mode Compil.find_error 
      Ocaml_mode.ocaml_find_error;
    Var.set_major_var mode Compil.find_error_location_regexp 
      (snd !!Ocaml_mode.ocaml_error_regexp);
  )
