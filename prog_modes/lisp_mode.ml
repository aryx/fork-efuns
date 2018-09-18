(***********************************************************************)
(*                                                                     *)
(*                           Efuns                                     *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
open Efuns
open Options
open Lisp_lexer

(***********************************************************************)
(*********************** colors ***********************)
(***********************************************************************)

let color_region buf start_point end_point =

  let _keyword_attr = 
    Text.make_attr (Attr.get_color !!Pl_colors.keyword_color) 1 0 false in
  let string_attr = 
    Text.make_attr (Attr.get_color !!Pl_colors.string_color) 1 0 false in
  let comment_attr = 
    Text.make_attr (Attr.get_color !!Pl_colors.comment_color) 1 0 false in
  let gray_attr = 
    Text.make_attr (Attr.get_color !!Pl_colors.module_color) 1 0 false in

  let text = buf.buf_text in
  let curseur = Text.new_point text in
  let lexbuf = lexing text start_point end_point in

  let rec iter prev_tok lexbuf =
    let (pos,len), token = token lexbuf in
    (match token with
        EOF _ -> raise Exit
      | COMMENT ->
          Text.set_position text curseur pos;
          Text.set_attrs text curseur len comment_attr
      | EOFSTRING
      | STRING ->
          Text.set_position text curseur pos;
          Text.set_attrs text curseur len string_attr
      | IDENT when prev_tok = QUOTE ->
          Text.set_position text curseur pos;
          Text.set_attrs text curseur len gray_attr            
      | _ -> ());
    iter token lexbuf
  in
  try
    iter COMMENT lexbuf
  with _ ->
    buf.buf_modified <- buf.buf_modified + 1;
    Text.remove_point text curseur

(***********************************************************************)
(************************  abbreviations ********************)
(***********************************************************************)

let abbreviations = []

(***********************************************************************)
(**********************  indentations *******************)
(***********************************************************************)

(* ??? copy-paste bug? *)
let start_regexp = Str.regexp "^\\(let\\|module\\|type\\|exception\\|open\\)";;

let pop_to_kwds = Indent.pop_to_kwds COMMENT
      
let rec parse lexbuf prev_tok stack eols indent indents =
  let _, token = token lexbuf in
  match token with
    EOL pos -> parse lexbuf prev_tok stack (pos::eols) indent indents
  | EOF pos -> Indent.fix indent  (pos :: eols) indents
  | EOFSTRING -> (0,[0]) :: (Indent.fix indent eols indents)
  | COMMENT -> parse lexbuf prev_tok stack [] indent
      (Indent.fix 0 eols indents) 

  | LBRACE          (* LBRACE ... RBRACE *)
  | LPAREN          (* LPAREN ... RPAREN *)
  | LBRACKET        (* LBRACKET ... RBRACKET  *)
    ->
      parse lexbuf token ((token,indent)::stack) [] (indent+2) 
      (Indent.fix indent eols indents)
  
(* Deterministic Terminators *) 
  | RPAREN ->
      (* find corresponding block delimiter *)
      let (stack,indent) = Indent.pop_to LPAREN stack in
      parse lexbuf token stack [] indent (Indent.fix indent eols indents)
  | RBRACE ->
      (* find corresponding block delimiter *)
      let (stack,indent) = Indent.pop_to LBRACE stack in
      parse lexbuf token stack [] indent (Indent.fix indent eols indents)
  | RBRACKET ->
      (* find corresponding block delimiter *)
      let (stack,indent) = Indent.pop_to LBRACKET stack in
      parse lexbuf token stack [] indent (Indent.fix indent eols indents)
  | _ ->
      parse lexbuf token stack [] indent 
        (Indent.fix indent eols indents)

let get_indentations pos lexbuf =
  parse lexbuf COMMENT [] [] 0 []

let print_exc e s =
  Printf.printf "Caught exception %s in %s" (Printexc.to_string e) s;
  print_newline ()

(* Now, use the indentation from the parser *)

let compute_indentations buf start_point end_point =
  let text = buf.buf_text in
  let curseur = Text.dup_point text start_point in
(* init indentation *)
  let _pos = Text.get_position text end_point in
  let lexbuf = lexing text curseur end_point in
  try
    let indentations = 
      get_indentations (Text.get_position text start_point) lexbuf in
    Text.remove_point text curseur;
    indentations
  with
    e ->
      Text.remove_point text curseur;
      raise e

let find_phrase_start buf curseur =
  let text = buf.buf_text in
  try
    let _ = Text.search_backward text start_regexp curseur in ()
  with
    Not_found -> Text.set_position text curseur 0

let indent_between_points buf start_point end_point =
  let text = buf.buf_text in
  let session = Text.start_session text in
  let curseur = Text.dup_point text start_point in
  try
    find_phrase_start buf curseur;
    let indentations = compute_indentations buf curseur end_point in
(* remove the Eof indentation *)
    let _,_,indentations = Indent.pop_indentations indentations in
(* indent other lines *)
    let rec iter indents =
      let (current,pos,indents) = Indent.pop_indentations indents in
      Text.set_position text curseur (pos+1);
      Indent.set_indent text curseur current;
      iter indents
    in
    iter indentations
  with
    e -> 
      Text.commit_session text session;
      Text.remove_point text curseur


(* Interactive: indent the current line, insert newline and indent next line *)
let insert_and_return frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
(* colors *)
  let start_point = Text.dup_point text point in
  Text.bmove text start_point (Text.point_to_bol text start_point);
  color_region buf start_point point;
  Text.remove_point text start_point;
(* indentations *)
  let curseur = Text.dup_point text point in
  try
    find_phrase_start buf curseur;
    let indentations = compute_indentations buf curseur point in
    Text.remove_point text curseur;
    let (next,pos,tail) = Indent.pop_indentations indentations in
    let current =
      try
        let (current, _, _ ) = Indent.pop_indentations tail in current
      with
        Not_found  -> 0
    in
    let session = Text.start_session text in
    Indent.set_indent text point current;
    Edit.insert_char frame '\n';
    Indent.set_indent text point next;
    Text.commit_session text session;
    Text.fmove text point next; 
    ()
  with
    e -> 
      Text.remove_point text curseur;
      Edit.insert_char frame '\n'

(* Interactive: indent the current line, insert newline and indent next line *)
let indent_current_line frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
(* colors *)
  let end_point = Text.dup_point text point in
  let start_point = Text.dup_point text point in
  Text.bmove text start_point (Text.point_to_bol text start_point);
  Text.fmove text end_point (Text.point_to_eol text end_point);
  color_region buf start_point end_point;
  Text.remove_point text start_point;
  Text.remove_point text end_point;
(* indentations *)
  let curseur = Text.dup_point text point in
  find_phrase_start buf curseur;
  let indentations = compute_indentations buf curseur point in
  Text.remove_point text curseur;
  let (next,pos,tail) = Indent.pop_indentations indentations in
  let current =
    try
      let (current, _, _ ) = Indent.pop_indentations tail in current
    with
      Not_found  -> 0
  in
  Indent.set_indent text point current

(***********************************************************************)
(**********************  find_error  *******************)
(***********************************************************************)

let error_regexp = Str.regexp 
  "File \"\\(.*\\)\", line \\([0-9]+\\), characters \\([0-9]+\\)-\\([0-9]+\\):"

(* more precise than Compil.find_error_gen, get column range *)
let find_error text error_point =
  let groups = 
    Text.search_forward_groups text error_regexp 
      error_point 4 in
  let error =
    { Compil.
      err_msg = Text.get_position text error_point;
      err_filename = groups.(0);
      err_line = (int_of_string groups.(1)) - 1;
      err_begin = int_of_string groups.(2);
      err_end = int_of_string groups.(3);
    } in
  Text.fmove text error_point 1;
  error
    
(***********************************************************************)
(*********************  installation ********************)
(***********************************************************************)

let install buf =
  Color.color_buffer_buf buf; 

  buf.buf_syntax_table.(Char.code '_') <- true;
  buf.buf_syntax_table.(Char.code '-') <- true;
  buf.buf_syntax_table.(Char.code '+') <- true;
  buf.buf_syntax_table.(Char.code '*') <- true;

  let abbrevs = Hashtbl.create 11 in
  Var.set_local buf Abbrevs.abbrev_table abbrevs;
  Utils.hash_add_assoc abbrevs abbreviations;
  ()


let mode = Ebuffer.new_major_mode "Lisp" (Some install)

let lisp_mode =
  Major_modes.enable_major_mode mode
[@@interactive]
  
let _ = 
  Hook.add_start_hook (fun () ->

    Var.set_major_var mode Compil.find_error find_error;
    Var.set_major_var mode Indent.indent_func indent_between_points;
    Var.set_major_var mode Color.color_func color_region;

    (*  add_major_key mode [c_c; ControlMap,Char.code 'e']
        "lisp-eval-buffer" eval_buffer;
    *)

    Keymap.add_major_key mode [NormalMap,XK.xk_Tab] indent_current_line;
    Keymap.add_major_key mode [NormalMap, XK.xk_Return] insert_and_return;
    ['}';']';')'] |> List.iter (fun char ->
      Keymap.add_major_key mode [NormalMap, Char.code char] (fun frame ->
        Edit.self_insert_command frame;
        Paren_mode.highlight_paren frame
      )
    );
    Var.add_global Ebuffer.modes_alist [(".*\\.\\(el\\|gwm\\|lisp\\)$", mode)];
  )
