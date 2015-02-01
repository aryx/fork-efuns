 
{
open Lexing 
  
type token =
| OPEN_TAG
| CLOSE_TAG
| DOCTYPE
| AMPERSAND
| COMMENT
| NORMAL
| ERROR
| EOF

  
let tokens = []
  
let token_to_string token =
  List.assoc token tokens

let lexer_start = ref 0
let position lexbuf =
  let b = lexeme_start lexbuf in
  let e = lexeme_end lexbuf in
  b + !lexer_start, e - b

let start_pos = ref 0
  
let end_pos lexbuf =
  let b = !start_pos in
  let e = lexeme_end lexbuf in
  b + !lexer_start, e - b

}

let blank = [ ' ' '\n' '\t' '\r' ]

rule token = parse
  | blank * { 
      token lexbuf }
  | "</"   { 
      start_pos := lexeme_start lexbuf;
      close_tag lexbuf }
  | "<!--" {       
      start_pos := lexeme_start lexbuf;
      comment lexbuf }
  | "<!" "<!" ['D' 'd']['O' 'o']['C' 'c']['T' 't']['Y' 'y']['P' 'p']['E' 'e'] 
      [^ '>']* '>' { 
      position lexbuf, DOCTYPE }
  | "<"    { 
      start_pos := lexeme_start lexbuf;
      open_tag lexbuf }
  | eof    { position lexbuf, EOF }
  | '&'    { 
      start_pos := lexeme_start lexbuf;
      ampersand lexbuf }
  | _      { token lexbuf }

and close_tag = parse
    ['a'-'z' 'A'-'Z' '0'-'9' '.' '-']+
    [^'>']* '>' { end_pos lexbuf, CLOSE_TAG }
  | _  { end_pos lexbuf, ERROR }

and open_tag = parse
    ['a'-'z' 'A'-'Z' '0'-'9' '.' '-']+ { attribs lexbuf }
  | _ { end_pos lexbuf, ERROR }

and attribs = parse
    [' ' '\t' '\n' '\r']+ { attribs lexbuf }
  | ['a'-'z' 'A'-'Z' '0'-'9' '.' '-' '_']+ 
      { tag_attrib lexbuf; attribs lexbuf }
  | '>' { end_pos lexbuf, OPEN_TAG }
  | _ { end_pos lexbuf, ERROR }

and tag_attrib = parse
    [' ' '\t' '\n' '\r']* '=' [' ' '\t' '\n' '\r']* { attribvalue lexbuf }
  | ""     { () }

and attribvalue = parse
    ['a'-'z' 'A'-'Z' '0'-'9' '.' '-']+       { () }
  | '"'       { inquote lexbuf }
  | '\''      { insingle lexbuf }
  | ""        { () }

and inquote = parse
    [^ '"' '&' '\027']+ { inquote lexbuf }
  | "\027\040\066" (* ASCII *)     { inquote lexbuf }
  | '"' { () }
  | '&' { let _ = ampersand lexbuf in inquote lexbuf }
  | ""  { () }

and insingle = parse
    [^ '\'' '&']+ { insingle lexbuf }
  | '\'' { () }
  | '&' { let _ = ampersand lexbuf in insingle lexbuf }
  | ""  { () }
  
and comment = parse
    "--"  [' ' '\t' '\r' '\n']* '>' { end_pos lexbuf, COMMENT }
  | eof  { end_pos lexbuf, ERROR }
  | _  { comment lexbuf }

and ampersand = parse
    '#' ['0'-'9']+ ';' { end_pos lexbuf, AMPERSAND }
  | ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']* ';'
      { end_pos lexbuf, AMPERSAND }
  (* terminating ; is not required if next character could not be 
     part of the lexeme *)
  | '#' ['0'-'9']+ { end_pos lexbuf, AMPERSAND }
  | ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*
    { end_pos lexbuf, AMPERSAND }
  | _ { end_pos lexbuf, ERROR }
    
{
(* val token : lexbuf -> token *)

(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Text
open Efuns
open Interactive
open Simple
open Select
open Compil
open Eval
open Complex
open Abbrevs  
open Keymap
open Window
  
let lexing text start_point end_point =
  lexer_start := get_position text start_point;
  Text.lexing text start_point end_point

(*********************** colors ***********************)
let html_color_region location buf start_point end_point =
  let red_attr = make_attr (get_color location "red") 1 0 false in
  let yellow_attr = make_attr (get_color location "yellow") 1 0 false in
  let blue_attr = make_attr (get_color location "blue") 1 0 false in
  let gray_attr = make_attr (get_color location "gray") 1 0 false in
  let text = buf.buf_text in
  let curseur = Text.add_point text in
  let lexbuf = lexing text start_point end_point in
  let rec iter prev_tok lexbuf =
    let (pos,len), token = token lexbuf in
    (match token with
        EOF  -> raise Exit
      | COMMENT ->
          set_position text curseur pos;
          set_attr text curseur len blue_attr
      | ERROR ->
          set_position text curseur pos;
          set_attr text curseur len blue_attr
      
      | OPEN_TAG
      | CLOSE_TAG ->
          set_position text curseur pos;
          set_attr text curseur len yellow_attr
      | AMPERSAND
      | DOCTYPE ->
          set_position text curseur pos;
          set_attr text curseur len gray_attr            
      | _ -> ());
    iter token lexbuf
  in
  try
    iter COMMENT lexbuf
  with
    _ ->
      buf.buf_modified <- buf.buf_modified + 1;
      remove_point text curseur

let html_color_buffer buf =
  let text = buf.buf_text in
  let start_point = Text.add_point text in
  let end_point = Text.add_point text in
  set_position text end_point (size text);
  html_color_region buf.buf_location buf start_point end_point;
  remove_point text start_point;
  remove_point text end_point

let html_color frame =
  html_color_buffer frame.frm_buffer

(************************  abbreviations ********************)

let abbreviations = []

(*********************  structures ********************)

let structures = [
    [c_c; NormalMap, Char.code 'h'], "<a href=\"^^\"> ^^ </a> ^^";
    [c_c; NormalMap, Char.code 'n'], "<a name=\"^^\"> ^^ </a> ^^";
    [c_c; NormalMap, Char.code 'b'], 
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\"
\"http://www.w3.org/TR/REC-html40/strict.dtd\">
<html> <head>
<title> ^^ </title>
</head>
<body>
^^
<address>
Fabrice Le Fessant<br>
Tel travail: 01 69 33 52 93<br>
<a href=\"mailto:fabrice.le_fessant@inria.fr\">fabrice.le_fessant@inria.fr</a>
</address>
<hr>
</body> </html>
";
    [c_c; NormalMap, Char.code '1'], "<H1> ^^ </H1> ^^";
    [c_c; NormalMap, Char.code '2'], "<H2> ^^ </H2> ^^";
    [c_c; NormalMap, Char.code '3'], "<H3> ^^ </H3> ^^";
    [c_c; NormalMap, Char.code '4'], "<H4> ^^ </H4> ^^";
    [c_c; NormalMap, Char.code '5'], "<H5> ^^ </H5> ^^";
    [c_c; NormalMap, Char.code 'l'], "<UL>\n<LI> ^^ </LI>\n^^ </UL> ^^";
    [c_c; NormalMap, Char.code 'i'], "<LI> ^^ </LI>\n ^^";
    ]
  
  
(*********************  installation ********************)

let c_c = (ControlMap,Char.code 'c')
let install buf =
  html_color_buffer buf; 
  buf.buf_syntax_table.(Char.code '_') <- true;
  buf.buf_syntax_table.(Char.code '-') <- true;
  buf.buf_syntax_table.(Char.code '+') <- true;
  buf.buf_syntax_table.(Char.code '*') <- true;
  Accents_mode.install buf;
  let abbrevs = Hashtbl.create 11 in
  set_local buf abbrev_table abbrevs;
  Utils.hash_add_assoc abbrevs abbreviations;
  install_structures buf structures;
  ()

let mode = Ebuffer.new_major_mode "HTML" [install]
let _ = 
  add_major_key mode [c_c; ControlMap,Char.code 'l']
  "html-color-buffer" (fun frame -> html_color_buffer frame.frm_buffer);
  let map = mode.maj_map in
  Keymap.add_binding map [NormalMap, Char.code ' '] 
    (fun frame ->
      expand_sabbrev frame;
      electric_insert_space frame);
  Keymap.add_binding map [MetaMap, Char.code 'q'] fill_paragraph;
  List.iter (fun char ->
      Keymap.add_binding map [NormalMap, Char.code char]
        (fun frame ->
          self_insert_command frame;
          highlight_paren frame)
  ) ['>']
  
let _ =  
  Efuns.add_start_hook (fun location ->
      add_interactive (location.loc_map) "html-mode" 
      (fun frame -> install frame.frm_buffer);
    let alist = get_global location Ebuffer.modes_alist in
    set_global location Ebuffer.modes_alist 
        ((".*\.\(html\|htm\)",mode)
        :: alist)
      )  
} 
