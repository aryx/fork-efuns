(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)


type token =
  | AMPERAMPER
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
  | EOF of Text.position
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
  | EOL of Text.position
  | EOFCOMMENT
  | EOFSTRING
  | ERROR
  | RULE
  | PARSE
  | DEF
  | LOC
val tokens : (token * string) list
val token_to_string : token -> string
val lexer_start : Text.position ref
val position : Lexing.lexbuf -> int * int
type lexer_error =
  | Illegal_character
  | Unterminated_comment
  | Unterminated_string
exception Error of int * int * lexer_error
val token : Lexing.lexbuf -> (Text.position * int) * token
val keyword_color : string Options.option_record
val string_color : string Options.option_record
val comment_color : string Options.option_record
val upper_color : string Options.option_record
val keyword_font : string Options.option_record
val string_font : string Options.option_record
val comment_font : string Options.option_record
val upper_font : string Options.option_record
val ocaml_color_region :
  Efuns.location -> Efuns.buffer -> Text.point -> Text.point -> unit
val ocaml_color_buffer : Efuns.buffer -> unit
val ocaml_color : Efuns.frame -> unit
type indentations = (int * Text.position list) list
val print_indentations : (int * int list) list -> unit
val print_stack : (token * int) list -> unit
val pop_to_top : (token * int) list -> (token * int) list * int
val pop_to : 'a -> ('a * int) list -> ('a * int) list * int
val pop_to_kwds :
  token list -> (token * int) list -> (token * int) list * token * int
val fix : 'a -> 'b list -> ('a * 'b list) list -> ('a * 'b list) list
val pop_indentation : ('a * 'b list) list -> 'a * 'b * ('a * 'b list) list
val token_offset : token -> int
val parse :
  Lexing.lexbuf ->
  token ->
  (token * int) list ->
  Text.position list ->
  int -> (int * Text.position list) list -> (int * Text.position list) list
val get_indentations : 'a -> Lexing.lexbuf -> (int * Text.position list) list
val print_exc : exn -> string -> unit
val compute_indentations :
  Efuns.buffer -> Text.point -> Text.point -> (int * Text.position list) list
val find_phrase_start : Efuns.buffer -> Text.point -> unit
val indent_between_points : Efuns.buffer -> Text.point -> Text.point -> unit
val indent_phrase : Efuns.frame -> unit
val indent_region : Efuns.frame -> unit
val indent_buffer : Efuns.frame -> unit
val insert_and_return : Efuns.frame -> unit
val indent_current_line : Efuns.frame -> unit
val split1 : string -> char -> string list
val parse_name : string -> string list
val find_long_word : Efuns.buffer -> Text.point -> string
val module_name : string -> string
val ocaml_find_error : Text.t -> Text.point -> Compil.error
val c_c : Efuns.key
val install : Efuns.buffer -> unit
val ocaml_path : string list Options.option_record
val find_env:  Efuns.buffer -> Text.point -> string list