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
(* Prelude *)
(***********************************************************************)
(* pad: was copy-pasted in the different lexers before 
 *
 * less: shared by multiple lexers so concurrency issues?
 *)

(***********************************************************************)
(* Globals *)
(***********************************************************************)

(* we usually lex just a region of the file, so this stores the start
 * of this region so position() below can return the correct global
 * position of a token
 *)
let lexer_start = ref 0

(***********************************************************************)
(* Helpers *)
(***********************************************************************)

let lexing text start_point end_point =
  lexer_start := Text.get_position text start_point;
  Text.lexing text start_point end_point


let position lexbuf =
  let b = Lexing.lexeme_start lexbuf in
  let e = Lexing.lexeme_end lexbuf in
  b + !lexer_start, e - b

type lexer_error =
  | Illegal_character
  | Unterminated_comment
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

(* Error report *)
  
exception Error of int * int * lexer_error

let report_error = function
    Illegal_character ->
      print_string "Illegal character"
  | Unterminated_comment ->
      print_string "Comment not terminated"
  | Unterminated_string ->
      print_string "String literal not terminated"
