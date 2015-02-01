{
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
open Options  
  open Lexing
  
  type token =
    IDENT
  | INT
  | FLOAT
  | STRING
  | ELLIPSIS
  | CHAR
  | SHARP
  | STAR
  | AMPERSAND
    
(* keywords *)    
  |  SIZEOF
  |  ENUM
  |  STRUCT
  |  UNION
  |  IF
  |  ELSE
  |  WHILE
  |  DO
  |  FOR
  |  SWITCH
  |  CASE
  |  DEFAULT
  |  BREAK
  |  CONTINUE
  |  RETURN
  |  GOTO
  |  TYPEOF
  |  ALIGNOF
  |  ATTRIBUTE
  |  EXTENSION
  |  LABEL
  (* operators *)
  | ASSIGN
  | PREFIXOP
  | INFIXOP

  | STATIC
  | EXTERN
    
    (* parenthesis *)
  | LBRACE
  | RBRACE
  | LPAREN
  | RPAREN
  | SEMI
  | COMMA
    
    (* Comments *)
  | COMMENT
  | EOFCOMMENT

    
  (* Macros *)
  | M_IFDEF
  | M_IFNDEF
  | M_DEFINE
  | M_ELSE
  | M_ENDIF
  | M_INCLUDE
    
  (* lines *)
  | EOL of (Text.position)
  | EOLMACRO
  | EOF of (Text.position)
  | EOFSTRING
  | ERROR
  
  
  let tokens = [
      IDENT,"IDENT";
      INT,"INT";
      FLOAT,"FLOAT";
      STRING,"STRING";
      ELLIPSIS,"ELLIPSIS";
      SIZEOF,"SIZEOF";
      ENUM,"ENUM";
      STRUCT,"STRUCT";
      UNION,"UNION";
      IF,"IF";
      ELSE,"ELSE";
      WHILE,"WHILE";
      DO,"DO";
      FOR,"FOR";
      SWITCH,"SWITCH";
      CASE,"CASE";
      DEFAULT,"DEFAULT";
      BREAK,"BREAK";
      CONTINUE,"CONTINUE";
      RETURN,"RETURN";
      GOTO,"GOTO";
      TYPEOF,"TYPEOF";
      ALIGNOF,"ALIGNOF";
      ATTRIBUTE,"ATTRIBUTE";
      EXTENSION,"EXTENSION";
      LABEL,"LABEL";
      ASSIGN,"ASSIGN";
      PREFIXOP,"PREFIXOP";
      INFIXOP,"INFIXOP";
      LBRACE,"LBRACE";
      RBRACE,"RBRACE";
      LPAREN,"LPAREN";
      RPAREN,"RPAREN";
      COMMENT,"COMMENT";
      EOFCOMMENT,"EOFCOMMENT";
      EOLMACRO,"EOLMACRO";
      EOFSTRING,"EOFSTRING";
      CHAR, "CHAR";
      SHARP, "SHARP";
      STAR, "STAR";
      AMPERSAND, "AMPERSAND";
      COMMA, "COMMA";
      SEMI, "SEMI";    
    ]
  
  
  
  let token_to_string token =
    List.assoc token tokens
  
  let lexer_start = ref 0
  let position lexbuf =
    let b = lexeme_start lexbuf in
    let e = lexeme_end lexbuf in
    b + !lexer_start, e - b
  
  type lexer_error =
    Illegal_character
  | Unterminated_comment
  | Unterminated_string

(* The table of keywords *)
  
  let keyword_table =
    let h = Hashtbl.create 149 in
    Utils.hash_add_assoc h [
      "sizeof",  SIZEOF;
      
      "enum", ENUM;
      "struct", STRUCT;
      "union",UNION;
      
      "if",IF;
      "else",ELSE;
      "while",WHILE;
      "do",DO;
      "for",FOR;
      "switch",SWITCH;
      "case",CASE;
      "default",DEFAULT;
      "break",BREAK;
      "continue",CONTINUE;
      "return",RETURN;
      "goto",GOTO;
      "typeof",TYPEOF;
      "label",LABEL;
      "static", STATIC;
      "extern", EXTERN;
    ];
    h
  
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
    | Unterminated_comment ->
        print_string "Comment not terminated"
    | Unterminated_string ->
        print_string "String literal not terminated"

}

let blank = [' ' '\009']
let return = ['\010' '\012' '\013']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar = 
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let identstart = 
  ['A'-'Z' 'a'-'z']
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
    
  | "#ifdef" { position lexbuf, M_IFDEF }
  | "#define" { position lexbuf, M_DEFINE }
  | "#ifndef" { position lexbuf, M_IFNDEF }
  | "#else" { position lexbuf, M_ELSE }
  | "#endif" { position lexbuf, M_ENDIF }
  | "#include" { position lexbuf, M_INCLUDE }    
    
  |  identstart  identchar *     
    { position lexbuf,
      let s = Lexing.lexeme lexbuf in
      try
        Hashtbl.find keyword_table s
      with Not_found ->
          IDENT }  
  | decimal_literal | hex_literal | oct_literal | bin_literal
      { position lexbuf, INT }
  | float_literal
      { position lexbuf, FLOAT }
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
    
  | "'" [^ '\\' '\''] "'"
      { position lexbuf, CHAR }
  | "'" '\\' ['\\' '\'' 'n' 't' 'b' 'r'] "'"
      { position lexbuf, CHAR }
  | "'" '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { position lexbuf, CHAR }
  | "#" { position lexbuf, SHARP }
  | "\\" return { position lexbuf, EOLMACRO }
  | "//" [ ^ '\n' ] * { position lexbuf, COMMENT }
  | "/*"
      { start_pos := Lexing.lexeme_start lexbuf;
      try
        comment lexbuf;
        (!start_pos + !lexer_start, lexeme_end lexbuf - !start_pos), COMMENT
      with
        Error (pos,len,error) -> (pos,len), EOFCOMMENT
    }
    
  | "--" | "++" { position lexbuf, PREFIXOP }
  | "==" | "+" | "-" | "||" | "&&" { position lexbuf, INFIXOP }
  | "=" | "+=" | "-=" | "*=" | "/=" { position lexbuf, ASSIGN }
  | "," { position lexbuf, COMMA }
  | ";" { position lexbuf, SEMI }
  | "*" { position lexbuf, STAR }
  | "&" { position lexbuf, AMPERSAND }
  | "{" { position lexbuf, LBRACE }
  | "}" { position lexbuf, RBRACE }
  | "(" { position lexbuf, LPAREN }
  | ")" { position lexbuf, RPAREN }
  | eof { let (pos,len) = position lexbuf in (pos,len), EOF pos }
  | _
      { position lexbuf, ERROR }

and comment = parse
    "*/" { () }
  | eof
      { raise
        (Error (!start_pos + !lexer_start, lexeme_end lexbuf - !start_pos,
          Unterminated_comment)) }
  | _ { comment lexbuf }

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



let c_regexp_string =
  "^\\([^:\n]+\\):\\([0-9]+\\):.*$"
let c_error_regexp = Str.regexp c_regexp_string


let c_find_error text error_point =
  let groups = 
    Text.search_forward_groups text c_error_regexp 
      error_point 2 in
  let error =
    {  
      err_msg = Text.get_position text error_point;
      err_filename = groups.(0);
      err_line = (int_of_string groups.(1)) - 1;
      err_begin = 0;
      err_end = 0;
    } in
  Text.fmove text error_point 1;
  error

(******************* couleurs *********************)

let c_color_region location buf start_point end_point =
  let red_attr = make_attr (get_color location "red") 1 0 false in
  let yellow_attr = make_attr (get_color location "cadetblue") 1 0 false in
  let blue_attr = make_attr (get_color location "blue") 1 0 false in
  let gray_attr = make_attr (get_color location "gray") 1 0 false in
  let text = buf.buf_text in
  let curseur = Text.add_point text in
  let lexbuf = lexing text start_point end_point in
  let rec iter lexbuf =
    let (pos,len), token = token lexbuf in
    (match token with
        EOF _ -> raise Exit
  |  SIZEOF
  |  ENUM
  |  STRUCT
  |  UNION
  |  IF
  |  ELSE
  |  WHILE
  |  DO
  |  FOR
  |  SWITCH
  |  CASE
  |  DEFAULT
  |  BREAK
  |  CONTINUE
  |  RETURN
  |  GOTO
  |  TYPEOF
  |  ALIGNOF
  |  ATTRIBUTE
  |  EXTENSION
      |  LABEL
      | STATIC
      | EXTERN
        -> 
          set_position text curseur pos;
          set_attr text curseur len red_attr
      | EOFCOMMENT 
      | COMMENT ->
          set_position text curseur pos;
          set_attr text curseur len blue_attr
      | EOFSTRING
      | CHAR 
      | STRING ->
          set_position text curseur pos;
          set_attr text curseur len yellow_attr
      | M_IFDEF
      | M_DEFINE
      | M_ELSE
      | M_IFNDEF
      | M_INCLUDE
      | M_ENDIF -> 
          set_position text curseur pos;
          set_attr text curseur len yellow_attr          
      | IDENT -> ()
      | _ -> ()
          );
    iter lexbuf
  in
  try
    iter lexbuf
  with
    _ ->
      buf.buf_modified <- buf.buf_modified + 1;
      remove_point text curseur

let c_color_buffer buf =
  let text = buf.buf_text in
  let start_point = Text.add_point text in
  let end_point = Text.add_point text in
  set_position text end_point (size text);
  c_color_region buf.buf_location buf start_point end_point;
  remove_point text start_point;
  remove_point text end_point

  (*********************** indentation ************************)
  
let start_regexp = Str.regexp "^{";;

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

let rec pop_to_top stack =
  match stack with
    [] -> ([],0)
  | _ :: stack -> pop_to_top stack

let rec pop_to kwd stack =
  match stack with
    [] -> ([],0)
  | (kwd',indent) :: stack when kwd' = kwd -> stack, indent
  | _ :: stack -> pop_to kwd stack

let rec pop_to_kwds kwds stack =
  match stack with
    [] -> ([],RBRACE, 0)
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

let token_offset prev_tok = 0
(*
  match prev_tok with
  
  | CHAR | GREATERRBRACE | GREATERRBRACKET | FALSE | FLOAT | INFIXOP0
  | INFIXOP1 | INFIXOP2 | INFIXOP3 | INFIXOP4 | INT | LESS | LESSMINUS
  | LIDENT | DONE | END | BARRBRACKET | UIDENT | UNDERSCORE | STRING 
  | PREFIXOP | QUESTION | QUOTE | RBRACE  | RBRACKET | RULE | PARSE
    -> 2
  
  | AMPERAMPER | AMPERSAND | AND | AS | ASSERT | BAR | BARBAR | BEGIN
  | CLASS | COLON | COLONCOLON | COLONEQUAL | COLONGREATER | COMMA 
  |   CONSTRAINT | DO | DOT | DOTDOT | DOWNTO | ELSE | EQUAL | EXCEPTION 
  | EXTERNAL | FOR | FUN | FUNCTION | FUNCTOR | GREATER | IF | IN 
  | INCLUDE | INHERIT | INITIALIZER | LAZY | LBRACE | LBRACELESS 
  | LBRACKET | LBRACKETBAR | LBRACKETLESS | LET | LPAREN | MATCH 
  | METHOD | MINUSGREATER | MODULE | MUTABLE | NEW | OBJECT | OF | OPEN
  | OR | PARSER | PRIVATE
  | REC | RPAREN | SEMI | SEMISEMI
  | SHARP | SIG | STAR | STRUCT | SUBTRACTIVE | THEN | TO | TRUE | TRY
  | TYPE | VAL | VIRTUAL | WHEN | WHILE | WITH
    -> 0
  
  | _ -> 0
*)
  
let rec parse lexbuf prev_tok stack eols indent indents =
  let _, token = token lexbuf in
  match token with
    EOL pos -> parse lexbuf prev_tok stack (pos::eols) indent indents
  | EOF pos -> fix indent  (pos :: eols) indents
  | EOFSTRING -> (0,[0]) :: (fix indent eols indents)
  | EOFCOMMENT -> (2,[0]) :: (fix indent eols indents)
  | COMMENT -> parse lexbuf prev_tok stack [] indent indents

      (* Terminated structures *)
  | LBRACE          (* LBRACE ... RBRACE *)
    ->
      let indent = 
        match stack with
          (IF, _ ) :: _
        | (FOR, _ ) :: _
        | (ELSE, _ ) :: _
        | (WHILE, _ ) :: _ -> indent - 2
        | _ -> indent
      in
      let offset = token_offset prev_tok in
      parse lexbuf token ((token,indent) :: stack) [] (indent+2) 
      (fix (indent+offset) eols indents)
  | LPAREN          (* LPAREN ... RPAREN *) ->
      let offset = token_offset prev_tok in
      parse lexbuf token ((token,indent) :: stack) [] (indent+2) 
      (fix (indent+offset) eols indents)
      
  | ELSE            (* ELSE { ... } *)
  | FOR             (* FOR (...) { ... }  *)
  | WHILE           (* WHILE (...) { ... }  *)
  | IF              (* IF (...) { ... } *)
    ->
      parse lexbuf token ((token,indent) :: stack) [] (indent+2) 
      (fix indent eols indents)
      
  | RBRACE -> (* This might terminate a IF/WHILE/FOR/ELSE structure *)
      let (stack,indent) = pop_to LBRACE stack in
      let (stack, indent) =
        match stack with
          (IF, indent) :: stack -> stack, indent
        | (FOR, indent) :: stack -> stack, indent
        | (ELSE, indent) :: stack -> stack, indent
        | (WHILE, indent) :: stack -> stack, indent
        | _ -> (stack, indent)
      in
      parse lexbuf token stack [] indent (fix indent eols indents)
      
(* Deterministic Terminators *) 
  | RPAREN ->
  (* find corresponding block delimiter *)
      let (stack,indent) = pop_to LPAREN stack in
      parse lexbuf token stack [] indent (fix indent eols indents)
      
  | SEMI ->
      let (stack, indent) =
        match stack with
          (IF, indent) :: stack -> stack, indent
        | (FOR, indent) :: stack -> stack, indent
        | (ELSE, indent) :: stack -> stack, indent
        | (WHILE, indent) :: stack -> stack, indent
        | _ -> (stack, indent)
      in 
      parse lexbuf token stack [] indent
        (fix indent eols indents)      
      
  | _ ->
      
      let offset = token_offset prev_tok in
      parse lexbuf token stack [] indent 
        (fix (indent+offset) eols indents)

let get_indentations pos lexbuf =
  parse lexbuf EOFCOMMENT [] [] 0 []

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
  c_color_region buf.buf_location buf start_point point;
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
  c_color_region buf.buf_location buf start_point end_point;
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

  
(************************  abbreviations ********************)

let abbreviations =
  []
  
(*********************  installation ********************)

let c_c = (ControlMap,Char.code 'c')
let install buf =
(*
  add_interactive (Buffer buf) "c-indent-buffer" indent_buffer;
  interactive (Buffer buf) [MetaMap,Char.code 'q']
    "c-indent-phrase" indent_phrase;
  interactive (Buffer buf) [NormalMap,XK.xk_Tab]
    "c-indent-line" indent_current_line;
  Keymap.add_binding map [NormalMap, XK.xk_Return] insert_and_return;
  *)
  c_color_buffer buf; 
  buf.buf_syntax_table.(Char.code '_') <- true;
  let abbrevs =
    try
      get_local (buf) abbrev_table
    with
      Failure _ -> 
        let abbrevs = Hashtbl.create 11 in
        set_local buf abbrev_table abbrevs;
        abbrevs
  in
  Utils.hash_add_assoc abbrevs abbreviations;
  ()

  
let mode = Ebuffer.new_major_mode "C"  [install]

let c_mode frame = Ebuffer.set_major_mode frame.frm_buffer mode
  
let _ =
  define_action "c_mode" c_mode;
  define_action "c_mode.color_buffer"
    (fun frame -> c_color_buffer frame.frm_buffer);
  define_action "c_mode.indent_buffer" indent_buffer;
  define_action "c_mode.compile"  (compile c_find_error);
  define_action "c_mode.indent_line" indent_current_line;
  define_action "c_mode.indent_phrase" indent_phrase
    
let mode_regexp = define_option ["c_mode"; "mode_regexp"] ""
    (list_option string_option) [ 
    ".*\.\(c\|cpp\|cc\|h\|H\|C\|y\|l\)$"
  ]
  
let local_map = define_option ["c_mode"; "local_map"] ""
    (list_option binding_option) []

let interactives_map = define_option ["c_mode"; "interactives_map"] ""
    (list_option string2_option) 
  []

  
let _ =
  let map = mode.maj_map in
  add_major_key (mode) [c_c; ControlMap, Char.code 'b'] 
    "c-indent-buffer" indent_buffer;
  add_major_key (mode) [MetaMap,Char.code 'q']
  "c-indent-phrase" indent_phrase;
  add_major_key (mode) [NormalMap,XK.xk_Tab]
  "c-indent-line" indent_current_line;
  
    List.iter (fun char ->
      Keymap.add_binding map [NormalMap, Char.code char]
        (fun frame ->
          self_insert_command frame;
          highlight_paren frame)
  ) ['}';']';')']

let _ =
  if !!local_map = [] then
    local_map =:= [
      [c_c;ControlMap, Char.code 'l'], "c_mode.color_buffer" ;
      [c_c;ControlMap, Char.code 'c'], "c_mode.compile" ;
      [MetaMap,Char.code 'q'], "c_mode.indent_phrase";
      [NormalMap,XK.xk_Tab], "c_mode.indent_line";
      [c_c; ControlMap, Char.code 'b'], "c_mode.indent_buffer";
  
      ];
  if !!interactives_map = [] then 
    interactives_map =:= [
      "color_buffer", "c_mode.color_buffer";
    ]

  
let _ =
  let map = mode.maj_map in
  (*  Keymap.add_prefix map [c_c]; *)
  List.iter (fun (keys, action) ->
      try
        let f = execute_action action in
        Keymap.add_binding map keys f;
        add_interactive map action f;
      with e ->
          Log.printf "Error for action %s" action;
          Log.exn "%s\n" e;  
  ) !!local_map;
  List.iter (fun (name, action) ->
      try
        add_interactive map name (execute_action action)
      with e ->
          Log.printf "Error for action %s" action;
          Log.exn "%s\n" e;          
  ) !!interactives_map;
  ()

let _ =  
  Efuns.add_start_hook (fun location ->
      add_interactive (location.loc_map) "c-mode" 
        (fun frame -> install frame.frm_buffer);
      let alist = get_global location Ebuffer.modes_alist in
      set_global location Ebuffer.modes_alist 
        ((List.map (fun s -> s,mode) !!mode_regexp) @ alist);
  )

}