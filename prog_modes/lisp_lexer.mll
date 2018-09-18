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
open Common_lexer
  
type token =
| IDENT

| COMMENT

| STRING
| NUM

| LBRACE | RBRACE
| LPAREN | RPAREN
| LBRACKET | RBRACKET

| DOT
| QUOTE
| COMMA

| EOF of (Text.position)
| EOL of (Text.position)
| EOFSTRING 

| ERROR
  
let start_pos = ref 0

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
      let _s = Lexing.lexeme lexbuf in
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
