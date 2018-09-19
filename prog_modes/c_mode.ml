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
open C_lexer

(***********************************************************************)
(* Find errors *)
(***********************************************************************)

(* Default stuff in compile.ml works *)

(***********************************************************************)
(******************* couleurs *********************)
(***********************************************************************)

let color_region buf start_point end_point =
  let keyword_attr = 
    Text.make_attr (Attr.get_color !!Pl_colors.keyword_color) 1 0 false in
  let string_attr = 
    Text.make_attr (Attr.get_color !!Pl_colors.string_color) 1 0 false in
  let comment_attr = 
    Text.make_attr (Attr.get_color !!Pl_colors.comment_color) 1 0 false in
  let _gray_attr = 
    Text.make_attr (Attr.get_color !!Pl_colors.module_color) 1 0 false in
  let preprocessor_attr = 
    Text.make_attr (Attr.get_color !!Pl_colors.preprocessor_color) 1 0 false in

  let text = buf.buf_text in
  let curseur = Text.new_point text in
  let lexbuf = Common_lexer.lexing text start_point end_point in

  let rec iter lexbuf =
    let (pos,len), token = C_lexer.token lexbuf in
    Text.set_position text curseur pos;
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
          Text.set_attrs text curseur len keyword_attr
      | EOFCOMMENT 
      | COMMENT ->
          Text.set_attrs text curseur len comment_attr
      | EOFSTRING
      | CHAR 
      | STRING ->
          Text.set_attrs text curseur len string_attr
      | M_IFDEF
      | M_DEFINE
      | M_ELSE
      | M_IFNDEF
      | M_INCLUDE
      | M_ENDIF -> 
          Text.set_attrs text curseur len preprocessor_attr          

      | IDENT -> ()
      | _ -> ()
          );
    iter lexbuf
  in
  try
    iter lexbuf
  with Exit ->
    buf.buf_modified <- buf.buf_modified + 1;
    Text.remove_point text curseur

(***********************************************************************)
(*********************** indentation ************************)
(***********************************************************************)
  
let start_regexp = Str.regexp "^{";;

let pop_to_kwds = Indent.pop_to_kwds RBRACE

let token_offset _prev_tok = 0
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
  
let rec parse lexbuf prev_tok  stack eols  indent indents =
  let _, token = C_lexer.token lexbuf in
  match token with
  | EOL pos -> parse lexbuf prev_tok stack (pos::eols) indent indents
  | EOF pos -> Indent.fix indent  (pos :: eols) indents
  | EOFSTRING -> (0,[0]) :: (Indent.fix indent eols indents)
  | EOFCOMMENT -> (2,[0]) :: (Indent.fix indent eols indents)
  (* ???? why discard eols when reach a comment? *)
  | COMMENT -> parse lexbuf prev_tok stack [] indent indents

      (* Terminated structures *)

      
  | IF              (* IF (...) { ... } *)
  | ELSE            (* ELSE { ... } *)
  | FOR             (* FOR (...) { ... }  *)
  | WHILE           (* WHILE (...) { ... }  *)
    ->
      parse lexbuf token ((token,indent) :: stack) [] (indent+2) 
        (Indent.fix indent eols indents)

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
        (Indent.fix (indent+offset) eols indents)
  | LPAREN          (* LPAREN ... RPAREN *) ->
      let offset = token_offset prev_tok in
      parse lexbuf token ((token,indent) :: stack) [] (indent+2) 
        (Indent.fix (indent+offset) eols indents)
      
  | RBRACE -> (* This might terminate a IF/WHILE/FOR/ELSE structure *)
      let (stack,indent) = Indent.pop_to LBRACE stack in
      let (stack, indent) =
        match stack with
          (IF, indent) :: stack -> stack, indent
        | (FOR, indent) :: stack -> stack, indent
        | (ELSE, indent) :: stack -> stack, indent
        | (WHILE, indent) :: stack -> stack, indent
        | _ -> (stack, indent)
      in
      parse lexbuf token stack [] indent (Indent.fix indent eols indents)
      
(* Deterministic Terminators *) 
  | RPAREN ->
      (* find corresponding block delimiter *)
      let (stack,indent) = Indent.pop_to LPAREN stack in
      parse lexbuf token stack [] indent (Indent.fix indent eols indents)
      
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
        (Indent.fix indent eols indents)      
      
  | _ ->      
      let offset = token_offset prev_tok in
      parse lexbuf token stack [] indent 
        (Indent.fix (indent+offset) eols indents)

(* could factorize this function more between the different major modes,
 * but not worth it; it complexifies things.
 *)
let get_indentations buf start_point end_point =
  let text = buf.buf_text in
  let lexbuf = Common_lexer.lexing text start_point end_point in
  parse lexbuf EOFCOMMENT [] [] 0 []


(* Now, use the indentation from the parser *)

let indent_between_points = 
  Indent.indent_between_points get_indentations start_regexp

let indent_current_line =
  Indent.indent_current_line get_indentations start_regexp color_region

(***********************************************************************)
(************************  abbreviations ********************)
(***********************************************************************)

let abbreviations =
  []
  
(***********************************************************************)
(*********************  installation ********************)
(***********************************************************************)

let install buf =
  (* uses color_region internally through Color.color_func *)
  Color.color_buffer_buf buf; 

  buf.buf_syntax_table.(Char.code '_') <- true;

  (* less: could be a major_var instead? *)
  let abbrevs =
    try
      Var.get_local (buf) Abbrevs.abbrev_table
    with Failure _ -> 
      let abbrevs = Hashtbl.create 11 in
      Var.set_local buf Abbrevs.abbrev_table abbrevs;
      abbrevs
  in
  Utils.hash_add_assoc abbrevs abbreviations;
  ()

  
let mode = Ebuffer.new_major_mode "C"  (Some install)

let c_mode = 
  Major_modes.enable_major_mode mode
[@@interactive]

(***********************************************************************)
(* Setup *)
(***********************************************************************)
  
let mode_regexp = define_option ["c_mode"; "mode_regexp"] ""
   (list_option string_option) [".*\\.\\(c\\|cpp\\|cc\\|h\\|H\\|C\\|y\\|l\\)$"]
let local_map = define_option ["c_mode"; "local_map"] ""
    (list_option Keymap.binding_option) []

let _ =  
  Hook.add_start_hook (fun () ->
    Var.add_global Ebuffer.modes_alist (List.map(fun s->s,mode) !!mode_regexp);

    Var.set_major_var mode Indent.indent_func indent_between_points;
    Var.set_major_var mode Color.color_func color_region;

    Keymap.add_major_key mode [NormalMap,XK.xk_Tab] indent_current_line;
  
    ['}';']';')'] |> List.iter (fun char ->
      Keymap.add_major_key mode [NormalMap, Char.code char] (fun frame ->
        Edit.self_insert_command frame;
        Paren_mode.highlight_paren frame
      )
  );

  !!local_map |> List.iter (fun (keys, action) ->
      try
        Keymap.add_major_key mode keys (Action.execute_action action)
      with e ->
        Log.printf "Error for action %s" action;
        Log.exn "%s\n" e;  
  );
  ()
  )
