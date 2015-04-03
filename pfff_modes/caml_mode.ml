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
    Pfff_modes.colorize funcs buf file
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
