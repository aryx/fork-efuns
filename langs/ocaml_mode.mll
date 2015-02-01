 
{
  open Lexing 
  
  
  type token =
    AMPERAMPER
  | AMPERSAND
  | AND
  | AS
  | ASSERT
  | BAR
  | BARBAR
  | BARRBRACKET
  | BEGIN
  | CHAR
  | CLASS
  | COLON
  | COLONCOLON
  | COLONEQUAL
  | COLONGREATER
  | COMMA
  | CONSTRAINT
  | DO
  | DONE
  | DOT
  | DOTDOT
  | DOWNTO
  | ELSE
  | END
  | EOF of (Text.position)
  | EQUAL
  | EXCEPTION
  | EXTERNAL
  | FALSE
  | FLOAT
  | FOR
  | FUN
  | FUNCTION
  | FUNCTOR
  | GREATER
  | GREATERRBRACE
  | GREATERRBRACKET
  | IF
  | IN
  | INCLUDE
  | INFIXOP0
  | INFIXOP1
  | INFIXOP2
  | INFIXOP3
  | INFIXOP4
  | INHERIT
  | INITIALIZER
  | INT
  | LAZY
  | LBRACE
  | LBRACELESS
  | LBRACKET
  | LBRACKETBAR
  | LBRACKETLESS
  | LESS
  | LESSMINUS
  | LET
  | LIDENT
  | LPAREN
  | MATCH
  | METHOD
  | MINUSGREATER
  | MODULE
  | MUTABLE
  | NEW
  | OBJECT
  | OF
  | OPEN
  | OR
  | PARSER
  | PREFIXOP
  | PRIVATE
  | QUESTION
  | QUOTE
  | RBRACE
  | RBRACKET
  | REC
  | RPAREN
  | SEMI
  | SEMISEMI
  | SHARP
  | SIG
  | STAR
  | STRING
  | STRUCT
  | SUBTRACTIVE
  | THEN
  | TO
  | TRUE
  | TRY
  | TYPE
  | UIDENT
  | UNDERSCORE
  | VAL
  | VIRTUAL
  | WHEN
  | WHILE
  | WITH
  | COMMENT
  | EOL of (Text.position)
  | EOFCOMMENT
  | EOFSTRING
  | ERROR
(* for lexers *)
  | RULE
  | PARSE
(* for JoCaml *)
  | DEF
  | LOC
  
  
  let tokens = [AMPERAMPER,"AMPERAMPER"; AMPERSAND,"AMPERSAND";
      AND,"AND"; AS,"AS"; ASSERT,"ASSERT"; BAR,"BAR"; BARBAR,"BARBAR";
      BARRBRACKET,"BARRBRACKET"; BEGIN,"BEGIN"; CHAR,"CHAR";
      CLASS,"CLASS"; COLON,"COLON"; COLONCOLON,"COLONCOLON";
      COLONEQUAL,"COLONEQUAL"; COLONGREATER,"COLONGREATER";
      COMMA,"COMMA"; CONSTRAINT,"CONSTRAINT"; DO,"DO"; DONE,"DONE";
      DOT,"DOT"; DOTDOT,"DOTDOT"; DOWNTO,"DOWNTO"; ELSE,"ELSE";
      END,"END"; EQUAL,"EQUAL"; EXCEPTION,"EXCEPTION";
      EXTERNAL,"EXTERNAL"; FALSE,"FALSE"; FLOAT,"FLOAT"; FOR,"FOR";
      FUN,"FUN"; FUNCTION,"FUNCTION"; FUNCTOR,"FUNCTOR";
      GREATER,"GREATER"; GREATERRBRACE,"GREATERRBRACE";
      GREATERRBRACKET,"GREATERRBRACKET"; IF,"IF"; IN,"IN";
      INCLUDE,"INCLUDE"; INFIXOP0,"INFIXOP0"; INFIXOP1,"INFIXOP1";
      INFIXOP2,"INFIXOP2"; INFIXOP3,"INFIXOP3"; INFIXOP4,"INFIXOP4";
      INHERIT,"INHERIT"; INITIALIZER,"INITIALIZER"; INT,"INT";
      LAZY,"LAZY"; LBRACE,"LBRACE"; LBRACELESS,"LBRACELESS";
      LBRACKET,"LBRACKET"; LBRACKETBAR,"LBRACKETBAR";
      LBRACKETLESS,"LBRACKETLESS"; LESS,"LESS"; LESSMINUS,"LESSMINUS";
      LET,"LET"; LIDENT,"LIDENT"; LPAREN,"LPAREN"; MATCH,"MATCH";
      METHOD,"METHOD"; MINUSGREATER,"MINUSGREATER"; MODULE,"MODULE";
      MUTABLE,"MUTABLE"; NEW,"NEW"; OBJECT,"OBJECT"; OF,"OF";
      OPEN,"OPEN"; OR,"OR"; PARSER,"PARSER"; PREFIXOP,"PREFIXOP";
      PRIVATE,"PRIVATE"; QUESTION,"QUESTION"; QUOTE,"QUOTE";
      RBRACE,"RBRACE"; RBRACKET,"RBRACKET"; REC,"REC"; RPAREN,"RPAREN";
      SEMI,"SEMI"; SEMISEMI,"SEMISEMI"; SHARP,"SHARP"; SIG,"SIG";
      STAR,"STAR"; STRING,"STRING"; STRUCT,"STRUCT";
      SUBTRACTIVE,"SUBTRACTIVE"; THEN,"THEN"; TO,"TO"; TRUE,"TRUE";
      TRY,"TRY"; TYPE,"TYPE"; UIDENT,"UIDENT"; UNDERSCORE,"UNDERSCORE";
      VAL,"VAL"; VIRTUAL,"VIRTUAL"; WHEN,"WHEN"; WHILE,"WHILE";
      WITH,"WITH"; COMMENT,"COMMENT"; EOFCOMMENT,"EOFCOMMENT";
      EOFSTRING,"EOFSTRING"; ERROR,"ERROR"; RULE,"RULE"; PARSE,"PARSE"]
  
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


(* For nested comments *)
  
  let comment_depth = ref 0

(* The table of keywords *)
  
  let keyword_table =
    let h = Hashtbl.create 149 in
    Utils.hash_add_assoc h [
      "and", AND;
      "as", AS;
      "assert", ASSERT;
      "begin", BEGIN;
      "class", CLASS;
      "constraint", CONSTRAINT;
      "do", DO;
      "done", DONE;
      "downto", DOWNTO;
      "else", ELSE;
      "end", END;
      "exception", EXCEPTION;
      "external", EXTERNAL;
      "false", FALSE;
      "for", FOR;
      "fun", FUN;
      "function", FUNCTION;
      "functor", FUNCTOR;
      "if", IF;
      "in", IN;
      "include", INCLUDE;
      "inherit", INHERIT;
      "initializer", INITIALIZER;
      "lazy", LAZY;
      "let", LET;
      "match", MATCH;
      "method", METHOD;
      "module", MODULE;
      "mutable", MUTABLE;
      "new", NEW;
      "object", OBJECT;
      "of", OF;
      "open", OPEN;
      "or", OR;
      "parser", PARSER;
      "private", PRIVATE;
      "rec", REC;
      "sig", SIG;
      "struct", STRUCT;
      "then", THEN;
      "to", TO;
      "true", TRUE;
      "try", TRY;
      "type", TYPE;
      "val", VAL;
      "virtual", VIRTUAL;
      "when", WHEN;
      "while", WHILE;
      "with", WITH;
      "mod", INFIXOP3;
      "land", INFIXOP3;
      "lor", INFIXOP3;
      "lxor", INFIXOP3;
      "lsl", INFIXOP4;
      "lsr", INFIXOP4;
      "asr", INFIXOP4;
(* for lexers *)
      "rule", RULE;
      "parse", PARSE;
    (* for JoCaml *)
      "loc", LOC;
      "def", DEF;
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
  | "_"
      { position lexbuf, UNDERSCORE }
  | lowercase identchar *
    { position lexbuf,
      let s = Lexing.lexeme lexbuf in
      try
        Hashtbl.find keyword_table s
      with Not_found ->
          LIDENT }
  | uppercase identchar *
    { position lexbuf, UIDENT }       (* No capitalized keywords *)
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
  | "(*"
      { comment_depth := 1;
      start_pos := Lexing.lexeme_start lexbuf;
      try
        comment lexbuf;
        (!start_pos + !lexer_start, lexeme_end lexbuf - !start_pos), COMMENT
      with
        Error (pos,len,error) -> (pos,len), EOFCOMMENT
    }
  | "#" [' ' '\t']* ['0'-'9']+ [' ' '\t']* "\"" [^ '\n' '\r'] *
    ('\n' | '\r' | "\r\n")
      (* # linenum "filename" flags \n *)
    { token lexbuf }
  | "#"  { position lexbuf, SHARP  }
  | "&"  { position lexbuf, AMPERSAND  }
  | "&&" { position lexbuf, AMPERAMPER  }
  | "'"  { position lexbuf, QUOTE  }
  | "("  { position lexbuf, LPAREN  }
  | ")"  { position lexbuf, RPAREN  }
  | "*"  { position lexbuf, STAR  }
  | ","  { position lexbuf, COMMA  }
  | "?"  { position lexbuf, QUESTION  }
  | "->" { position lexbuf, MINUSGREATER  }
  | "."  { position lexbuf, DOT  }
  | ".." { position lexbuf, DOTDOT  }
  | ":"  { position lexbuf, COLON  }
  | "::" { position lexbuf, COLONCOLON  }
  | ":=" { position lexbuf, COLONEQUAL  }
  | ":>" { position lexbuf, COLONGREATER  }
  | ";"  { position lexbuf, SEMI  }
  | ";;" { position lexbuf, SEMISEMI  }
  | "<"  { position lexbuf, LESS  }
  | "<-" { position lexbuf, LESSMINUS  }
  | "="  { position lexbuf, EQUAL  }
  | "["  { position lexbuf, LBRACKET  }
  | "[|" { position lexbuf, LBRACKETBAR  }
  | "[<" { position lexbuf, LBRACKETLESS  }
  | "]"  { position lexbuf, RBRACKET  }
  | "{"  { position lexbuf, LBRACE  }
  | "{<" { position lexbuf, LBRACELESS  }
  | "|"  { position lexbuf, BAR  }
  | "||" { position lexbuf, BARBAR  }
  | "|]" { position lexbuf, BARRBRACKET  }
  | ">"  { position lexbuf, GREATER  }
  | ">]" { position lexbuf, GREATERRBRACKET  }
  | "}"  { position lexbuf, RBRACE  }
  | ">}" { position lexbuf, GREATERRBRACE  }
  
  | "!=" { position lexbuf, INFIXOP0 }
  | "-"  { position lexbuf, SUBTRACTIVE }
  | "-." { position lexbuf, SUBTRACTIVE }
  
  | ['!' '?' '~'] symbolchar *
    { position lexbuf, PREFIXOP }
  | ['=' '<' '>' '|' '&' '$'] symbolchar *
    { position lexbuf, INFIXOP0 }
  | ['@' '^'] symbolchar *
    { position lexbuf, INFIXOP1 }
  | ['+' '-'] symbolchar *
    { position lexbuf, INFIXOP2 }
  | "**" symbolchar *
    { position lexbuf, INFIXOP4 }
  | ['*' '/' '%'] symbolchar *
    { position lexbuf, INFIXOP3 }
  | eof { let (pos,len) = position lexbuf in (pos,len), EOF pos }
  | _
      { position lexbuf, ERROR }

and comment = parse
    "(*"
    { comment_depth := succ !comment_depth; comment lexbuf }
  | "*)"
      { comment_depth := pred !comment_depth;
      if !comment_depth > 0 then comment lexbuf }
  | "\""
      { reset_string_buffer();
      let old_start_pos = !start_pos in
      start_pos := Lexing.lexeme_start lexbuf;
      string lexbuf;
      string_buff := initial_string_buffer;
      start_pos := old_start_pos;
      comment lexbuf }
  | "''"
      { comment lexbuf }
  | "'" [^ '\\' '\''] "'"
      { comment lexbuf }
  | "'\\" ['\\' '\'' 'n' 't' 'b' 'r'] "'"
      { comment lexbuf }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { comment lexbuf }
  | eof
      { raise
        (Error (!start_pos + !lexer_start, lexeme_end lexbuf - !start_pos,
          Unterminated_comment)) }
  | _
      { comment lexbuf }

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

open Options
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

let keyword_color = define_option ["ocaml_mode"; "keyword_color"] ""
    string_option "red"
let string_color = define_option ["ocaml_mode"; "string_color"] ""
    string_option "blue"
let comment_color = define_option ["ocaml_mode"; "comment_color"] ""
    string_option "cadetblue"
let upper_color = define_option ["ocaml_mode"; "upper_color"] ""
    string_option "blue"

let keyword_font = define_option ["ocaml_mode"; "keyword_font"] ""
    string_option !!font
let string_font = define_option ["ocaml_mode"; "string_font"] ""
    string_option !!font
let comment_font = define_option ["ocaml_mode"; "comment_font"] ""
  string_option !!font
let upper_font = define_option ["ocaml_mode"; "upper_font"] ""
  string_option !!font

let ocaml_path = define_option ["ocaml_mode"; "ocaml_path"] ""
    path_option []
  
let _ =
  if !!ocaml_path = [] then
    ocaml_path =:= !!Efuns.load_path
  
(*********************** colors ***********************)
let ocaml_color_region location buf start_point end_point =
  let red_attr = make_attr (get_color location !!keyword_color) 1 
      (get_font location !!keyword_font) false in
  let yellow_attr = make_attr (get_color location !!string_color) 1 
      (get_font location !!string_font)    false in
  let blue_attr = make_attr (get_color location !!comment_color) 1 
      (get_font location !!comment_font) false in
  let gray_attr = make_attr (get_color location !!upper_color) 1 
      (get_font location !!upper_font)     false in
  let text = buf.buf_text in
  let curseur = Text.add_point text in
  let lexbuf = lexing text start_point end_point in
  let rec iter lexbuf =
    let (pos,len), token = token lexbuf in
    (match token with
        EOF _ -> raise Exit
      | LET | IN | MATCH | TRY | WITH | FUN 
      | FUNCTION | IF | THEN | ELSE | WHILE | WHEN
      | DONE | DO | FOR | TO | DOWNTO | BEGIN | END 
      | OPEN | MODULE | STRUCT | MUTABLE
      | AND | OR | TYPE | VAL | CLASS | SIG | INHERIT | OBJECT
      | EXCEPTION | RULE | METHOD | EXTERNAL -> 
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
      | UIDENT ->
          set_position text curseur pos;
          set_attr text curseur len gray_attr            
      | _ -> ());
    iter lexbuf
  in
  try
    iter lexbuf
  with
    _ ->
      buf.buf_modified <- buf.buf_modified + 1;
      remove_point text curseur



let ocaml_color_buffer buf =
  let text = buf.buf_text in
  Text.unset_attr text;
  let start_point = Text.add_point text in
  let end_point = Text.add_point text in
  set_position text end_point (size text);
  ocaml_color_region buf.buf_location buf start_point end_point;
  remove_point text start_point;
  remove_point text end_point

let ocaml_color frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let start_point = Text.add_point text in
  let end_point = Text.add_point text in
  set_position text end_point (size text);
  ocaml_color_region buf.buf_location buf start_point end_point;
  remove_point text start_point;
  remove_point text end_point

(************************  abbreviations ********************)

let abbreviations = define_option ["ocaml_mode"; "abbrevs"] ""
    (list_option string2_option) []
  
let _ = 
  if !!abbreviations = [] then
    abbreviations =:=
      [
(* Pervasives *)
      ( "nl'", "print_newline ()");
      ( "pr'i", "print_int");
(* Printexc *)
      ( "to'p","Printexc.to_string");
(* Printf *)
      ( "pr'","Printf.printf");
      ( "spr'", "Printf.sprintf");
      ( "fpr'", "Printf.fprintf");
(* List *)
      ( "rev'l","List.rev");
      ( "ite'l","List.iter");
      ( "len'l","List.length");
      ( "lfol'l","List.fold_left");
      ( "rfol'l","List.fold_right");
      ( "map'l","List.map");
      ( "mem'l","List.mem");
      ( "memq'l","List.memq");
      ( "mass'l","List.mem_assoc");
      ( "assq'l","List.assq");
      ( "spl'l","List.split");
      ( "com'l","List.combine");
(* Array *)
      ( "ite'a","Array.iter");
      ( "len'a","Array.length");
      ( "set'a","Array.set");
      ( "get'a","Array.get");
      ( "cre'a","Array.create");
      ( "ini'a","Array.init");
      ( "cop'a","Array.copy");
      ( "sub'a","Array.sub");
      ( "map'a","Array.map");
      ( "con'a", "Array.concat");
(* Hashtbl *)
      ( "ite'h","Hashtbl.iter");
      ( "fin'h","Hashtbl.find");
      ( "add'h","Hashtbl.add");
      ( "rem'h","Hashtbl.remove");
      ( "cre'h","Hashtbl.create");
      ( "cle'h","Hashtbl.clear");
(* Queue *)
      ( "ite'q","Queue.iter");
      ( "len'q","Queue.length");
      ( "cre'q","Queue.create");
      ( "add'q","Queue.add");
      ( "get'q","Queue.take");
      ( "tak'q","Queue.take");
      ( "cle'q","Queue.clear");
      ( "emp'q","Queue.Empty");
(* Filename *)
      ( "con'f","Filename.concat");
      ( "che'f","Filename.check_suffix");
      ( "cho'f","Filename.chop_suffix");
      ( "bas'f","Filename.basename");
      ( "dir'f","Filename.dirname");
(* String *)
      ( "len's","String.length");
      ( "set's","String.set");
      ( "get's","String.get");
      ( "cre's","String.create");
      ( "mak's","String.make");
      ( "sub's","String.sub");
      ( "cop's","String.copy");
      ( "bli's","String.blit");
      ( "con's","String.concat");
      ( "low's","String.lowercase");
      ( "upp's","String.uppercase");
      ( "cap's","String.capitalize");
      ( "unc's","String.uncapitalize");
(* Char *)
      ( "upp'c","Char.uppercase");
      ( "low'c","Char.lowercase");
(* Mutex *)
      ( "loc'm","Mutex.lock");
      ( "unl'm","Mutex.unlock");
      ( "cre'm","Mutex.create");
      ( "cre'c","Condition.create");
      ( "wai'c","Condition.wait");
      ( "cre't","Thread.create");
      ( "len'w","Weak.length");
      ( "set'w","Weak.set");
      ( "get'w","Weak.get");
      ( "a'","Array");
      ( "c'","Char");
      ( "d'","Digest");
      ( "f'","Filename");
      ( "h'","Hashtbl");
      ( "l'","List");
      ( "m'","Map");
      ( "o'","Obj");
      ( "p'","Printexc");
      ( "q'","Queue");
      ( "r'","Random");
      ( "s'","String");
      ( "t'","Thread");
      ( "w'","Weak");
    ]
    

(**********************  indentations *******************)
  
let start_regexp = define_option ["ocaml_mode"; "start_regexp"]
    "" regexp_option (
    string_to_regex "^\(let\|module\|type\|exception\|open\)");;

type indentations = (int * (Text.position list)) list
let indentation = define_option ["ocaml_mode"; "indentation"] ""
  int_option 2
  
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
  | (STRUCT,indent) :: _ -> stack, indent+ !!indentation
  | (SIG,indent) :: _ -> stack, indent+ !!indentation
  | (LBRACE,indent) :: _ -> stack, indent+ !!indentation
  | (OBJECT,indent) :: _ -> stack, indent+ !!indentation
  | _ :: stack -> pop_to_top stack

let rec pop_to kwd stack =
  match stack with
    [] -> ([],0)
  | (kwd',indent) :: stack when kwd' = kwd -> stack, indent
  | _ :: stack -> pop_to kwd stack

let rec pop_to_kwds kwds stack =
  match stack with
    [] -> ([],SEMISEMI, 0)
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

let token_offset prev_tok =
  match prev_tok with
  
  | CHAR | GREATERRBRACE | GREATERRBRACKET | FALSE | FLOAT | INFIXOP0
  | INFIXOP1 | INFIXOP2 | INFIXOP3 | INFIXOP4 | INT | LESS | LESSMINUS
  | LIDENT | DONE | END | BARRBRACKET | UIDENT | UNDERSCORE | STRING 
  | PREFIXOP | QUESTION | QUOTE | RBRACE  | RBRACKET | RULE | PARSE
    ->  !!indentation
  
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

let rec parse lexbuf prev_tok stack eols indent indents =
  let _, token = token lexbuf in
  match token with
    EOL pos -> parse lexbuf prev_tok stack (pos::eols) indent indents
  | EOF pos -> fix indent  (pos :: eols) indents
  | EOFSTRING -> (0,[0]) :: (fix indent eols indents)
  | EOFCOMMENT -> ( !!indentation,[0]) :: (fix 0 eols indents)
  | COMMENT -> parse lexbuf prev_tok stack [] indent (fix 0 eols indents)
  | LET ->
      (* 
  indentation des LETs: Il faut savoir s'il s'agit d'un LET avec ou sans IN.
   Pour cela, on regarde simplement le token precedent.
   Voici ceux qui ne peuvent pas introduire un TOP-LET
*)
      begin
        match prev_tok with
          IN | THEN | COLONCOLON | INFIXOP0 | INFIXOP0 | INFIXOP1 |
          INFIXOP2 | INFIXOP3 | INFIXOP4 | SUBTRACTIVE | STAR | EQUAL | LESS |
          GREATER | OR | BARBAR | AMPERAMPER | AMPERSAND | COLONEQUAL |
          LESSMINUS | LPAREN | LBRACKET | LBRACKETBAR | MATCH | TRY | IF |
          WHILE | DO | TO | DOWNTO | BEGIN | MINUSGREATER | WHEN | COMMA |
          SEMI | QUESTION | QUOTE | BAR ->
            (* On reste dans le bloc precedent, donc avec la meme indentation *)
            parse lexbuf token ((token,indent) :: stack) [] (indent+ !!indentation) 
            (fix indent eols indents)
        
        | ELSE ->
            (* On reste dans le bloc precedent, mais avec une indentation plus
        petite car on est sorti du IF THEN ELSE *)
            parse lexbuf token ((token,indent- !!indentation) :: stack) [] indent 
              (fix (indent- !!indentation) eols indents)
        
        | _ ->
            (* On est dans un nouveau LET toplevel *)
            let (stack, indent) = pop_to_top stack in
            parse lexbuf token ((token,indent) :: stack) [] (indent+ !!indentation) 
            (fix indent eols indents)
      
      end
  
  | VAL | EXTERNAL | TYPE | EXCEPTION | OPEN
  | INCLUDE | CLASS | RULE | METHOD | INITIALIZER | VIRTUAL ->
      (* On est dans une pharse toplevel *)
      let (stack, indent) = pop_to_top stack in
      parse lexbuf token ((token,indent) :: stack) [] (indent+ !!indentation) 
      (fix indent eols indents)
  
  | SEMISEMI ->
      let (stack,indent) = pop_to_top stack in
      parse lexbuf token stack [] indent (fix indent eols indents)
  
  | MODULE ->
      if prev_tok = LET then
        (* LET MODULE *) 
        parse lexbuf token stack [] indent (fix indent eols indents)
      else
        (* On est dans une pharse toplevel *)
      let (stack, indent) = pop_to_top stack in
      parse lexbuf token ((token,indent) :: stack) [] (indent+ !!indentation) 
      (fix indent eols indents)
  
  | EQUAL ->
      let (stack',kwd,indent') = pop_to_kwds [DEF; BAR] stack in
      (* if we find a DEF, we are the first = after the DEF, ie a process
      follows. We put a BAR to prevent any other EQUAL to match this DEF.
  Other EQUALs should not be affected by this JoCaml need. *)
      
      if kwd = DEF then
        parse lexbuf token 
          ((BAR,indent)::stack) [] indent (fix indent eols indents)
      else
        parse lexbuf token stack [] indent (fix indent eols indents)
  
  | AND ->
      let (stack,kwd,indent) = pop_to_kwds 
          [LET;TYPE;RULE;CLASS;DEF;LOC] stack in
      parse lexbuf token ((kwd,indent)::stack)
      [] (indent+ !!indentation) (fix indent eols indents) 
  | OR ->
      let (stack',kwd,indent') = pop_to_kwds  [DEF] stack in
      if kwd = DEF then
        parse lexbuf token stack
          [] (indent'+ !!indentation) (fix indent' eols indents)
      else
        parse lexbuf token stack
          [] indent (fix indent eols indents)
  
  | IN -> 
      (* partially terminate a LET structure *)
      let (stack,indent) = pop_to LET stack in
      parse lexbuf token ((IN,indent)::stack)
      [] indent (fix indent eols indents)
  
  | DEF
  | LOC -> 
      parse lexbuf token ((token,indent)::stack)
      [] (indent+ !!indentation) (fix indent eols indents)
  
  | DO ->
(* starts a DO ... DONE structure *)
      let (stack',kwd,indent') = pop_to_kwds [WHILE;FOR;LOC] stack in
      if kwd = LOC then
      (* LOC ... DO { ... } *)
        parse lexbuf DO stack [] (indent'+ !!indentation) 
        (fix indent' eols indents)
      else
        parse lexbuf DO ((DO,indent') :: stack') [] (indent'+ !!indentation) 
        (fix indent' eols indents)
(* These keywords start multi-keywords block structures. *)

(* This symbol has different meanings in lexer files *)
  | LBRACE          (* LBRACE ... RBRACE *)
    ->
      if prev_tok = RBRACE &&
        (match stack with
            (BAR,_) :: _ 
          | (PARSE,_) :: _
            -> true
          | _ -> false) then
        parse lexbuf SEMISEMI [] [] 0 
          (fix 0 eols indents)
      else
      let offset = token_offset prev_tok in
      parse lexbuf token ((token,indent) :: stack) [] (indent+ !!indentation) 
      (fix (indent+offset) eols indents)

(* Terminated structures *)
  | LPAREN          (* LPAREN ... RPAREN *)
  | LBRACELESS      (* LBRACELESS ... GREATERRBRACE  *)
  | LBRACKET        (* LBRACKET ... RBRACKET  *)
  | LBRACKETBAR     (* LBRACKETBAR ... BARRBRACKET *)
  | LBRACKETLESS    (* LBRACKETLESS ... GREATERRBRACKET *)
  | BEGIN           (* BEGIN ... END  *)
    ->
      let offset = token_offset prev_tok in
      parse lexbuf token ((token,indent) :: stack) [] (indent+ !!indentation) 
      (fix (indent+offset) eols indents)
      | COLON
    ->
      let offset = token_offset prev_tok in
      parse lexbuf token ((token,indent) :: stack) [] indent
        (fix (indent+offset) eols indents)
  
  | STRUCT          (* STRUCT ... END *)
  | SIG             (* SIG ... END *)
  | FOR             (* FOR ... TO/DOWNTO... DO  ... DONE  *)
  | WHILE           (* WHILE ... DO ... DONE  *)
  | OBJECT
    
(* Non-terminated structures *)
  | MATCH           (*  MATCH ... WITH ...  *)
  | TRY             (*  TRY ... WITH ...  *)
  | FUNCTION        (*  FUNCTION ... MINUSGREATER ...  *)
  | FUN             (*  FUN ... MINUSGREATER ...  *)
  | PARSER          (*  PARSER ... MINUSGREATER ...  *)
  | IF              (*  TRY ... WITH ...  *)
    ->
      begin
        match prev_tok with
          ELSE ->
            (* On reste dans le bloc precedent, mais avec une indentation plus
            petite car on est sorti du IF THEN ELSE *)
            parse lexbuf token ((token,indent- !!indentation) :: stack) [] indent 
              (fix (indent- !!indentation) eols indents)
        
        | _ ->
            parse lexbuf token ((token,indent) :: stack) [] (indent+ !!indentation) 
            (fix indent eols indents)
      end
      
(* Deterministic Terminators *) 
  | RPAREN 
  | RBRACE
  | GREATERRBRACE 
  | RBRACKET 
  | BARRBRACKET 
  | GREATERRBRACKET ->
      (* find corresponding block delimiter *)
      let kwd = List.assoc token 
          [
          RPAREN, LPAREN;
          RBRACE, LBRACE;
          RBRACKET,LBRACKET;
          BARRBRACKET, LBRACKETBAR;
          GREATERRBRACE, LBRACELESS;
          GREATERRBRACKET, LBRACKETLESS
        ]
      in
      let (stack,indent) = pop_to kwd stack in
      parse lexbuf token stack [] indent (fix indent eols indents)
      (* Non-deterministic terminators *)
  | END 
  | DONE
    ->
      let kwds = List.assoc token 
          [
          END,[BEGIN;STRUCT;SIG];
          DONE, [FOR;WHILE;DO;TO;DOWNTO]
        ] 
      in
      let (stack,kwd, indent) = pop_to_kwds kwds stack in
      parse lexbuf token stack [] indent (fix indent eols indents)
  | WITH ->
      let (stack,kwd,indent) = pop_to_kwds [MATCH;TRY;LBRACE] stack in
      if kwd = LBRACE then
        parse lexbuf token ((LBRACE,indent)::stack) [] (indent+ !!indentation)
        (fix indent eols indents)        
      else
        parse lexbuf token ((WITH,indent)::stack) [] (indent+ !!indentation)
        (fix indent eols indents)
  | BAR ->
      let (stack,kwd,indent) = 
        pop_to_kwds [WITH;FUNCTION;BAR;TYPE;PARSE;LPAREN;LBRACE;DEF] stack in
      let kwd = 
        match kwd with
          TYPE | LPAREN | PARSE | LBRACE | DEF -> kwd
        | _ -> BAR
      in
      parse lexbuf token ((kwd,indent)::stack) [] (indent+ !!indentation)
      (fix indent eols indents)
  | MINUSGREATER ->
      let (stack,kwd,indent) =
        pop_to_kwds [WITH;FUN;BAR;FUNCTION;TYPE;LPAREN;EXTERNAL;VAL;COLON] stack in
      begin
        match kwd with
          TYPE | LPAREN | EXTERNAL | VAL | COLON ->
            let offset = token_offset prev_tok in
            parse lexbuf token ((kwd,indent)::stack) [] indent 
              (fix (indent+offset) eols indents)
        | _ ->
            parse lexbuf token ((BAR,indent)::stack) [] 
              (if kwd = FUN then indent+ !!indentation else indent+ 2 * !!indentation)
            (fix (indent+ !!indentation) eols indents) 
      end
  | THEN ->
      let (stack,indent) = pop_to IF stack in
      parse lexbuf token ((THEN,indent)::stack) [] (indent+ !!indentation)
      (fix indent eols indents) 
  | ELSE ->
      let (stack,indent) = pop_to THEN stack in
      parse lexbuf token ((ELSE,indent)::stack) [] (indent+ !!indentation)
      (fix indent eols indents) 
  | SEMI ->
      let old_stack = stack in
(* le ; termine un THEN ... ou ELSE ... s'il n'y a pas 
   construction infinie (LET, IN, MATCH, BAR) avant *)
      let (stack1,_,indent1) = pop_to_kwds [THEN;ELSE] stack in
      let (stack2,_,_) = pop_to_kwds
          [
          LET; IN; COLONCOLON; INFIXOP0; INFIXOP0; INFIXOP1; INFIXOP2;
          INFIXOP3; INFIXOP4; SUBTRACTIVE; STAR; EQUAL; LESS; GREATER;
          OR; BARBAR; AMPERAMPER; AMPERSAND; COLONEQUAL; LESSMINUS;
          LPAREN; LBRACKET; LBRACKETBAR; MATCH; TRY; IF; WHILE; DO; TO;
          DOWNTO; BEGIN; MINUSGREATER; WHEN; COMMA; SEMI; QUESTION;
          QUOTE; BAR; LBRACE
        ]
          stack in
      let new_stack, new_indent =
        if List.length stack1 > List.length stack2 then 
(* le THEN ou ELSE est en premier *)
          stack1, indent1
        else
        (* on continue tout simplement *)
          stack, indent
      in
      parse lexbuf token new_stack [] new_indent
        (fix indent eols indents)
  
  | PARSE           (* RULE ... PARSE ... *)
    ->
      begin
        match stack with
          (RULE,_) :: _ ->
            parse lexbuf token ((token,indent) :: stack) [] indent 
              (fix indent eols indents)
        | _ ->
            let offset = token_offset prev_tok in
            parse lexbuf token stack [] indent 
              (fix (indent+offset) eols indents)
      end
  | _ ->
      
      let offset = token_offset prev_tok in
      parse lexbuf token stack [] indent 
        (fix (indent+offset) eols indents)

let get_indentations pos lexbuf =
  parse lexbuf SEMISEMI [] [] 0 []

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
    let _ = Text.search_backward text (snd !!start_regexp) curseur in ()
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
  ocaml_color_region buf.buf_location buf start_point point;
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
  ocaml_color_region buf.buf_location buf start_point end_point;
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

(*********************  aide a la programmation *********)

  (* split a string (remove chr) *)
let split1 str chr =
  let list = ref [] in
  let start = ref 0 in
  for i = 0 to String.length str - 1 do
    if str.[i] = chr then
      let s = String.sub str !start (i - !start) in
      if s <> "" then list := s :: !list;
      start := i+1
  done;
  let s = String.sub str !start (String.length str - !start) in
  if s <> "" then list := s :: !list;
  List.rev !list


let parse_name str = split1 str '.'

(* open a minibuffer for a long name (module+name), then look
 in the corresponding .cmi file to find the type of the value *)



let find_long_word buf point =
  buf.buf_syntax_table.(Char.code '.') <- true;
  let w = current_word buf point in
  buf.buf_syntax_table.(Char.code '.') <- false;
  w  
  
  (*
  A rudimentary parser, to find all OPEN directives. This will be
  useful when trying to discover where a name is defined.
  In particular, we will open each .cmi file to find the name.
  *)

let module_name buf_name = 
  Filename.chop_extension (String.capitalize buf_name)

let find_env buf point =
  let text = buf.buf_text in
  let tmp_point = add_point text in
  let rec parse lexbuf stack env =
    let _, t = token lexbuf in
    match t with
      STRUCT -> parse lexbuf ((STRUCT,env) :: stack) env
    | OPEN -> 
        let (pos,len),t = token lexbuf in
        if t <> UIDENT then
          parse lexbuf stack env
        else
          (set_position text tmp_point pos; 
            let ident = Text.sub text tmp_point len in
            parse lexbuf stack (ident::env))
    | END ->
        let stack,env = 
          match stack with
            (STRUCT,env) :: tail -> tail, env
          | _ :: tail -> tail, env
          | [] -> [], env
        in
        parse lexbuf stack env
    | EOFSTRING 
    | EOFCOMMENT
    | EOF _ -> env
    | _ -> parse lexbuf stack env
  in
  let end_point = dup_point text point in
  let curseur = add_point text in
  let lexbuf = lexing text curseur end_point in
  let env = parse lexbuf [] [] in
  remove_point text curseur;
  remove_point text tmp_point;
  remove_point text end_point;
  (module_name buf.buf_name) :: env

(* C-f1 : approximatively parse the file to find the implementation for
the word under the cursor. During parsing, an envirronment is built and
then used to find the word. 
*)

let ocaml_error_regexp = define_option ["ocaml_mode"; "error_regexp"] ""
    regexp_option (string_to_regex
    "File \"\\(.*\\)\", line \\([0-9]+\\), characters \\([0-9]+\\)[-]\\([0-9]*\\):")
  
let ocaml_find_error text error_point =
  let groups = 
    Text.search_forward_groups text (snd !!ocaml_error_regexp) 
      error_point 4 in
  let error =
    {  
      err_msg = Text.get_position text error_point;
      err_filename = groups.(0);
      err_line = (int_of_string groups.(1)) - 1;
      err_begin = int_of_string groups.(2);
      err_end = try
        int_of_string groups.(3)
      with
        _ -> int_of_string groups.(2)
    } in
  Text.fmove text error_point 1;
  error

  (*********************  structures ********************)
let c_c = (ControlMap,Char.code 'c')

let structures = define_option ["ocaml_mode"; "structures"] ""
    (list_option binding_option) []
  
let _ = 
  if !!structures = [] then
    let n' = NormalMap,Char.code '\'' in
    structures =:=
    [
      [c_c; n'; NormalMap, Char.code 'b'], "begin ^^ end^^";
      [c_c; n'; NormalMap, Char.code 'f'], "for ^^ to ^^ do\n^^";
      [c_c; n'; NormalMap, Char.code 'w'], "while ^^ do\n^^\ndone";
      [c_c; n'; NormalMap, Char.code 't'], "try\n ^^ \nwith\n ^^";
      [c_c; n'; NormalMap, Char.code 'm'], "match ^^ with\n ^^";
      [c_c; n'; NormalMap, Char.code 'i'], "if ^^ then\n ^^\n else\n ^^";
      [c_c; n'; NormalMap, Char.code 'l'], "let ^^ = ^^ in\n ^^";
    ]    
  
(*********************  installation ********************)

let syntax = define_option ["ocaml_mode"; "syntax"] 
    "Chars which should not are part of idents" 
    string_option "_\'"
  
  
let ocaml_hooks = define_option ["ocaml_mode"; "hooks"] "" 
  (list_option string_option)
  [  "paren_mode" ]
  
let install buf =
  ocaml_color_buffer buf; 
  let syntax = !!syntax in
  for i = 0 to String.length syntax - 1 do
    buf.buf_syntax_table.(Char.code syntax.[i]) <- true;
  done;
  let abbrevs = Hashtbl.create 11 in
  set_local buf abbrev_table abbrevs;
  Utils.hash_add_assoc abbrevs !!abbreviations;
  install_structures buf !!structures;
    List.iter (fun action ->
      try execute_buffer_action action buf with _ -> ()
  ) !!ocaml_hooks;
  ()

  
let mode =  Ebuffer.new_major_mode "Ocaml" [install]
let map = mode.maj_map

let ocaml_mode frame = Ebuffer.set_major_mode frame.frm_buffer mode
        
let _ = 
  define_action "ocaml_mode" ocaml_mode;
  define_action "ocaml_mode.compile" (compile ocaml_find_error);
  define_action "ocaml_mode.color_buffer" 
    (fun frame -> ocaml_color_buffer frame.frm_buffer);
  define_action "ocaml_mode.indent_buffer" indent_buffer;
  define_action "ocaml_mode.eval_buffer" eval_buffer;
  define_action "ocaml_mode.indent_phrase" indent_phrase;
  define_action "ocaml_mode.indent_line" indent_current_line;
  define_action "ocaml_mode.char_expand_abbrev" (fun frame ->
      expand_sabbrev frame; self_insert_command frame);
  define_action "ocaml_mode.return_expand_abbrev"
    (fun frame -> expand_sabbrev frame; insert_and_return frame); 
  ()
  
let local_map = define_option ["ocaml_mode"; "local_map"] ""
    (list_option binding_option) []

let interactives_map = define_option ["ocaml_mode"; "interactives_map"] ""
    (list_option string2_option) 
  []

let _ =
  if !!local_map = [] then
    local_map =:= [
      [c_c; ControlMap, Char.code 'c'], "ocaml_mode.compile";    
      [ControlMap, Char.code 'l'], "ocaml_mode.color_buffer";
      [c_c; ControlMap, Char.code 'b'], "ocaml_mode.indent_buffer";
      [c_c; ControlMap, Char.code 'C'], "ocaml_mode.color_buffer";
      [c_c; ControlMap,Char.code 'e'], "ocaml_mode.eval_buffer";
      [c_c; ControlMap,Char.code 'l'], "ocaml_mode.color_buffer";
      [MetaMap,Char.code 'q'], "ocaml_mode.indent_phrase";
      [NormalMap,XK.xk_Tab], "ocaml_mode.indent_line";
      [NormalMap, Char.code '.'], "ocaml_mode.char_expand_abbrev";
      [NormalMap, Char.code ';'], "ocaml_mode.char_expand_abbrev";
      [NormalMap, XK.xk_Return], "ocaml_mode.return_expand_abbrev";

    ];
  if !!interactives_map = [] then 
        interactives_map =:= [
          "compile", "ocaml_mode.compile";
          "color_buffer", "ocaml_mode.color_buffer";
      ]

let _ =
  let map = mode.maj_map in
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

let mode_regexp = define_option ["ocaml_mode"; "mode_regexp"] ""
    (list_option string_option) [".*\.\(ml\|mli\|mll\|mly\|mlp\|mlg\)"]
  
let _ =  
  Efuns.add_start_hook (fun location ->
      let alist = get_global location Ebuffer.modes_alist in
      set_global location Ebuffer.modes_alist 
        ((List.map (fun s -> s,mode) !!mode_regexp) @ alist);
      add_option_parameter location keyword_color;
      add_option_parameter location string_color;
      add_option_parameter location comment_color;
      add_option_parameter location upper_color;
      add_option_parameter location keyword_font;
      add_option_parameter location string_font;
      add_option_parameter location comment_font;
      add_option_parameter location upper_font;
      add_option_parameter location ocaml_path;
      add_option_parameter location indentation;
  )  
  (*** Ocaml minor mode (for Makefiles (!)) ***)

let minor_mode = Ebuffer.new_minor_mode "ocaml" []
  
let _ =
  ignore (Keymap.add_binding minor_mode.min_map 
      [c_c; ControlMap, Char.code 'c'] (execute_action "ocaml_mode.compile"))
  ;
  
  define_action "ocaml_minor_mode" 
    (fun frame -> 
      let buf = frame.frm_buffer in
      if Ebuffer.modep buf minor_mode then begin
          Ebuffer.del_minor_mode buf minor_mode
        end else
        Ebuffer.set_minor_mode buf minor_mode)

} 
