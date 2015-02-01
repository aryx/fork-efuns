 
{
open Lexing 
  
type token =
  IDENT
| COMMENT
| STRING
| LBRACE
| RBRACE
| LPAREN
| RPAREN
| LBRACKET
| RBRACKET
| NUM
| DOT
| QUOTE
| COMMA
| EOF of (Text.position)
| EOL of (Text.position)
| EOFSTRING 
| ERROR
  
let tokens = []
  
let token_to_string token =
  List.assoc token tokens

let lexer_start = ref 0
let position lexbuf =
  let b = lexeme_start lexbuf in
  let e = lexeme_end lexbuf in
  b + !lexer_start, e - b

type lexer_error =
    Illegal_character
  | Unterminated_string

(* To buffer string literals *)

let initial_string_buffer = String.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0

let store_string_char c =
  if !string_index >= String.length (!string_buff) then begin
    let new_buff = String.create (String.length (!string_buff) * 2) in
      String.blit (!string_buff) 0 new_buff 0 (String.length (!string_buff));
      string_buff := new_buff
  end;
  String.unsafe_set (!string_buff) (!string_index) c;
  incr string_index

let get_stored_string () =
  let s = String.sub (!string_buff) 0 (!string_index) in
  string_buff := initial_string_buffer;
  s

(* To translate escape sequences *)

let char_for_backslash =
  match Sys.os_type with
  | "Unix" | "Win32" ->
      begin function
      | 'n' -> '\010'
      | 'r' -> '\013'
      | 'b' -> '\008'
      | 't' -> '\009'
      | c   -> c
      end
  | "MacOS" ->
      begin function
      | 'n' -> '\013'
      | 'r' -> '\010'
      | 'b' -> '\008'
      | 't' -> '\009'
      | c   -> c
      end
  | x -> failwith "Lexer: unknown system type"

let char_for_decimal_code lexbuf i =
  let c = 100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
           10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in  
  Char.chr(c land 0xFF)

(* To store the position of the beginning of a string or comment *)

let start_pos = ref 0

(* Error report *)

exception Error of int * int * lexer_error
let report_error = function
    Illegal_character ->
      print_string "Illegal character"
  | Unterminated_string ->
      print_string "String literal not terminated"

}

let blank = [' ' '\009']
let return = ['\010' '\012' '\013']
let firstidentchar = 
  ['A'-'Z' 'a'-'z' '-' '_' '\192'-'\214' '\216'-'\246'
    '\248'-'\255']
let identchar = 
  ['A'-'Z' 'a'-'z' '-' '_' '\192'-'\214' '\216'-'\246'
    '\248'-'\255' '0'-'9']
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let decimal_literal = ['0'-'9']+
let hex_literal = '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
let oct_literal = '0' ['o' 'O'] ['0'-'7']+
let bin_literal = '0' ['b' 'B'] ['0'-'1']+
let float_literal =
  ['0'-'9']+ ('.' ['0'-'9']*)? (['e' 'E'] ['+' '-']? ['0'-'9']+)?

rule token = parse
    blank +
      { token lexbuf }
  | return { let (p,_) as pos = position lexbuf in pos, EOL p }
  | firstidentchar identchar *
      { position lexbuf,
      let s = Lexing.lexeme lexbuf in
      IDENT }
  | float_literal
  | decimal_literal | hex_literal | oct_literal | bin_literal
      { position lexbuf, NUM }
  | "\""
      { reset_string_buffer();
        let string_start = Lexing.lexeme_start lexbuf in
        start_pos := string_start;
        try
          string lexbuf;
          lexbuf.Lexing.lex_start_pos <-
             string_start - lexbuf.Lexing.lex_abs_pos;
          (!start_pos + !lexer_start, lexeme_end lexbuf - !start_pos), STRING
        with
          Error (pos,len,error) -> (pos,len), EOFSTRING          
      }
  | (* comments *)
    ";"+ [^ '\n']* '\n' { position lexbuf, COMMENT }
    (* symbols *)
  | "'"  { position lexbuf, QUOTE  }
  | "("  { position lexbuf, LPAREN  }
  | ")"  { position lexbuf, RPAREN  }
  | ","  { position lexbuf, COMMA  }
  | "."  { position lexbuf, DOT  }
  | "["  { position lexbuf, LBRACKET  }
  | "]"  { position lexbuf, RBRACKET  }
  | "{"  { position lexbuf, LBRACE  }
  | "}"  { position lexbuf, RBRACE  }
  | symbolchar *
            { position lexbuf, IDENT }
  | eof { let (pos,len) = position lexbuf in (pos,len), EOF pos }
  | _
      { position lexbuf, ERROR }

and string = parse
    '"'
      { () }
  | '\\' ("\010" | "\013" | "\013\010") [' ' '\009'] *
      { string lexbuf }
  | '\\' ['\\' '"' 'n' 't' 'b' 'r']
      { store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
        string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { store_string_char(char_for_decimal_code lexbuf 1);
         string lexbuf }
  | eof
      { raise
          (Error (!start_pos + !lexer_start, lexeme_end lexbuf - !start_pos,
                 Unterminated_string)) }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf }

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
let lisp_color_region location buf start_point end_point =
  let red_attr = make_attr (get_color location "red") 1 0 false in
  let yellow_attr = make_attr (get_color location "black") 1 0 false in
  let blue_attr = make_attr (get_color location "blue") 1 0 false in
  let gray_attr = make_attr (get_color location "gray") 1 0 false in
  let text = buf.buf_text in
  let curseur = Text.add_point text in
  let lexbuf = lexing text start_point end_point in
  let rec iter prev_tok lexbuf =
    let (pos,len), token = token lexbuf in
    (match token with
        EOF _ -> raise Exit
      | COMMENT ->
          set_position text curseur pos;
          set_attr text curseur len blue_attr
      | EOFSTRING
      | STRING ->
          set_position text curseur pos;
          set_attr text curseur len yellow_attr
      | IDENT when prev_tok = QUOTE ->
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

let lisp_color_buffer buf =
  let text = buf.buf_text in
  let start_point = Text.add_point text in
  let end_point = Text.add_point text in
  set_position text end_point (size text);
  lisp_color_region buf.buf_location buf start_point end_point;
  remove_point text start_point;
  remove_point text end_point

let lisp_color frame =
  lisp_color_buffer frame.frm_buffer

(************************  abbreviations ********************)

let abbreviations = []

(**********************  indentations *******************)

let start_regexp = Str.regexp "^\(let\|module\|type\|exception\|open\)";;

type indentations = (int * (Text.position list)) list

let print_indentations list =
  print_string "Indentations :"; print_newline ();
  List.iter (fun (indent, list) ->
    List.iter (fun pos -> 
      Printf.printf "Line at %d with %d" pos indent
        ) list
   ) list;
  print_newline ()

let print_stack stack =
  print_string "Indentation stack:"; print_newline ();
  let rec iter stack =
    match stack with
      [] -> ()
    | (token, indent) :: stack ->
        Printf.printf "Token %s indent %d" 
          (List.assoc token tokens) indent;
        print_newline ();
        iter stack
  in
  iter stack

let rec pop_to kwd stack =
  match stack with
    [] -> ([],0)
  | (kwd',indent) :: stack when kwd' = kwd -> stack, indent
  | _ :: stack -> pop_to kwd stack
      
let rec pop_to_kwds kwds stack =
  match stack with
    [] -> ([],COMMENT, 0)
  | (kwd,indent) :: stack when List.memq kwd kwds -> 
      stack, kwd, indent
  | _ :: stack -> pop_to_kwds kwds stack
      
let fix indent eols indents =
  match eols with
    [] -> indents
  | _ -> 
      match indents with
        (pindent,peols) :: tail when pindent = indent ->
          (indent, eols @ peols) :: tail
      | _ ->  (indent,eols) :: indents

let rec pop_indentation indents =
  match indents with
    [] -> raise Not_found
  | (indent, eols) :: indents ->
      match eols with
        [] -> pop_indentation indents
      | eol :: eols ->
          (indent, eol, (indent,eols) :: indents)


let rec parse lexbuf prev_tok stack eols indent indents =
  let _, token = token lexbuf in
  match token with
    EOL pos -> parse lexbuf prev_tok stack (pos::eols) indent indents
  | EOF pos -> fix indent  (pos :: eols) indents
  | EOFSTRING -> (0,[0]) :: (fix indent eols indents)
  | COMMENT -> parse lexbuf prev_tok stack [] indent
      (fix 0 eols indents) 

  | LBRACE          (* LBRACE ... RBRACE *)
  | LPAREN          (* LPAREN ... RPAREN *)
  | LBRACKET        (* LBRACKET ... RBRACKET  *)
    ->
      parse lexbuf token ((token,indent)::stack) [] (indent+2) 
      (fix indent eols indents)
  
(* Deterministic Terminators *) 
  | RPAREN ->
      (* find corresponding block delimiter *)
      let (stack,indent) = pop_to LPAREN stack in
      parse lexbuf token stack [] indent (fix indent eols indents)
  | RBRACE ->
      (* find corresponding block delimiter *)
      let (stack,indent) = pop_to LBRACE stack in
      parse lexbuf token stack [] indent (fix indent eols indents)
  | RBRACKET ->
      (* find corresponding block delimiter *)
      let (stack,indent) = pop_to LBRACKET stack in
      parse lexbuf token stack [] indent (fix indent eols indents)
  | _ ->
      parse lexbuf token stack [] indent 
        (fix indent eols indents)

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
  let pos = get_position text end_point in
  let lexbuf = lexing text curseur end_point in
  try
    let indentations = 
      get_indentations (get_position text start_point) lexbuf in
    remove_point text curseur;
    indentations
  with
    e ->
      remove_point text curseur;
      raise e

let find_phrase_start buf curseur =
  let text = buf.buf_text in
  try
    let _ = Text.search_backward text start_regexp curseur in ()
  with
    Not_found -> Text.set_position text curseur 0

let indent_between_points buf start_point end_point =
  let text = buf.buf_text in
  let session = start_session text in
  let curseur = dup_point text start_point in
  try
    find_phrase_start buf curseur;
    let indentations = compute_indentations buf curseur end_point in
(* remove the Eof indentation *)
    let _,_,indentations = pop_indentation indentations in
(* indent other lines *)
    let rec iter indents =
      let (current,pos,indents) = pop_indentation indents in
      set_position text curseur (pos+1);
      set_indent text curseur current;
      iter indents
    in
    iter indentations
  with
    e -> 
      commit_session text session;
      remove_point text curseur

(* Interactive: indent all lines of the current block *)
let indent_phrase frame =
  let buf = frame.frm_buffer in
  let point = frame.frm_point in
  indent_between_points buf point point

let indent_region frame =
  let buf = frame.frm_buffer in
  let point = frame.frm_point in
  let mark = Ebuffer.get_mark buf point in
  let (start_point,end_point) =
    if point < mark then (point,mark) else (mark,point) 
  in
  indent_between_points buf start_point end_point
  

let indent_buffer frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let start_point = add_point text in
  let end_point = add_point text in
  set_position text end_point (Text.size text);
  indent_between_points buf start_point end_point;
  remove_point text start_point;
  remove_point text end_point

(* Interactive: indent the current line, insert newline and indent next line *)
let insert_and_return frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
(* colors *)
  let start_point = dup_point text point in
  bmove text start_point (point_to_bol text start_point);
  lisp_color_region buf.buf_location buf start_point point;
  remove_point text start_point;
(* indentations *)
  let curseur = dup_point text point in
  try
    find_phrase_start buf curseur;
    let indentations = compute_indentations buf curseur point in
    remove_point text curseur;
    let (next,pos,tail) = pop_indentation indentations in
    let current =
      try
        let (current, _, _ ) = pop_indentation tail in current
      with
        Not_found  -> 0
    in
    let session = start_session text in
    set_indent text point current;
    insert_char frame '\n';
    set_indent text point next;
    commit_session text session;
    fmove text point next; 
    ()
  with
    e -> 
      remove_point text curseur;
      insert_char frame '\n'

(* Interactive: indent the current line, insert newline and indent next line *)
let indent_current_line frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
(* colors *)
  let end_point = dup_point text point in
  let start_point = dup_point text point in
  bmove text start_point (point_to_bol text start_point);
  fmove text end_point (point_to_eol text end_point);
  lisp_color_region buf.buf_location buf start_point end_point;
  remove_point text start_point;
  remove_point text end_point;
(* indentations *)
  let curseur = dup_point text point in
  find_phrase_start buf curseur;
  let indentations = compute_indentations buf curseur point in
  remove_point text curseur;
  let (next,pos,tail) = pop_indentation indentations in
  let current =
    try
      let (current, _, _ ) = pop_indentation tail in current
    with
      Not_found  -> 0
  in
  set_indent text point current

let lisp_regexp_string =
  "File \"\\(.*\\)\", line \\([0-9]+\\), characters \\([0-9]+\\)-\\([0-9]+\\):"
let lisp_error_regexp = Str.regexp lisp_regexp_string


let lisp_find_error text error_point =
  let groups = 
    Text.search_forward_groups text lisp_error_regexp 
      error_point 4 in
  let error =
    {  
      err_msg = Text.get_position text error_point;
      err_filename = groups.(0);
      err_line = (int_of_string groups.(1)) - 1;
      err_begin = int_of_string groups.(2);
      err_end = int_of_string groups.(3);
    } in
  Text.fmove text error_point 1;
  error
    
(*********************  installation ********************)

let c_c = (ControlMap,Char.code 'c')
let install buf =
  lisp_color_buffer buf; 
  buf.buf_syntax_table.(Char.code '_') <- true;
  buf.buf_syntax_table.(Char.code '-') <- true;
  buf.buf_syntax_table.(Char.code '+') <- true;
  buf.buf_syntax_table.(Char.code '*') <- true;
  let abbrevs = Hashtbl.create 11 in
  set_local buf abbrev_table abbrevs;
  Utils.hash_add_assoc abbrevs abbreviations;
  ()


let mode = Ebuffer.new_major_mode "Lisp" [install]  
  
let _ =
  let map = mode.maj_map in
  add_interactive map "lisp-indent-buffer" indent_buffer;
  add_interactive map "lisp-color-buffer" 
    (fun frame -> lisp_color_buffer frame.frm_buffer);
  add_major_key mode [c_c; ControlMap, Char.code 'c'] 
  "lisp-compile" (compile lisp_find_error);
  add_major_key mode [c_c; ControlMap,Char.code 'e']
  "lisp-eval-buffer" eval_buffer;
  add_major_key mode [c_c; ControlMap,Char.code 'l']
    "lisp-color-buffer" (fun frame -> lisp_color_buffer frame.frm_buffer);
  add_major_key mode [MetaMap,Char.code 'q']
    "lisp-indent-phrase" indent_phrase;
  add_major_key mode [NormalMap,XK.xk_Tab]
    "lisp-indent-line" indent_current_line;
  Keymap.add_binding map [NormalMap, XK.xk_Return] insert_and_return;
  List.iter (fun char ->
      Keymap.add_binding map [NormalMap, Char.code char]
        (fun frame ->
          self_insert_command frame;
          highlight_paren frame)
      ) ['}';']';')']

let _ =  
  Efuns.add_start_hook (fun location ->
      add_interactive location.loc_map "lisp-mode" 
        (fun frame -> install frame.frm_buffer);
      let alist = get_global location Ebuffer.modes_alist in
    set_global location Ebuffer.modes_alist 
        ((".*\.\(el\|gwm\)$",mode)
       :: alist)
      )  
} 
