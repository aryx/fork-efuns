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

(***********************************************************************)
(* Lexing *)
(***********************************************************************)

open Lexing
  
type token =
| IDENT
| INT
| FLOAT
| STRING
| ELLIPSIS
| CHAR
| SHARP
| STAR
| AMPERSAND
    
(* keywords *)    
| SIZEOF
| ENUM
| STRUCT
| UNION
| IF
| ELSE
| WHILE
| DO
| FOR
| SWITCH
| CASE
| DEFAULT
| BREAK
| CONTINUE
| RETURN
| GOTO
| TYPEOF
| ALIGNOF
| ATTRIBUTE
| EXTENSION
| LABEL
(* operators *)
| ASSIGN
| PREFIXOP
| INFIXOP

| STATIC
| EXTERN
  
(* parenthesis *)
| LBRACE | RBRACE
| LPAREN | RPAREN
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
    | "Unix" | "Cygwin" ->
      begin function
        | 'n' -> '\010'
        | 'r' -> '\013'
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

let lexing text start_point end_point =
  lexer_start := Text.get_position text start_point;
  Text.lexing text start_point end_point

}
