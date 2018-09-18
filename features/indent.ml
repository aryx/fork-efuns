(*s: features/indent.ml *)
(*s: copyright header efuns *)
(***********************************************************************)
(*                                                                     *)
(*                             Efuns                                   *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
(*e: copyright header efuns *)
open Efuns

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Mostly helpers functions to build programming language indenters.
 *
 * pad: I factorized code between all the prog_modes/xxx_mode.ml
 *)

(*****************************************************************************)
(* Indent actions *)
(*****************************************************************************)

(* pad: I factorized some indent code by using a variable *)
let indent_func = Store.create_abstr "Indent.indent_func"

let get_indent_func buf =
  Var.get_var buf indent_func

(* indent all lines of the current block *)
let indent_phrase frame =
  let (buf, _, point) = Frame.buf_text_point frame in
  (get_indent_func buf) buf point point
[@@interactive]

let indent_region frame =
  let (buf, _, point) = Frame.buf_text_point frame in
  let mark = Ebuffer.get_mark buf point in
  let (start_point,end_point) =
    if point < mark then (point,mark) else (mark,point) 
  in
   (get_indent_func buf) buf start_point end_point
[@@interactive]


(* alt: C-x h and then M-x color_region *)
let indent_buffer frame =
  let (buf, text, _) = Frame.buf_text_point frame in
  Text.with_new_point text (fun start_point ->
  Text.with_new_point text (fun end_point ->
  Text.set_position text end_point (Text.size text);
  (get_indent_func buf) buf start_point end_point;
  ))
[@@interactive]

(*****************************************************************************)
(* get_indentations (defined by each major mode) helpers *)
(*****************************************************************************)

(* indent level and list of eols *)
type indentations = (int * (Text.position list)) list

type 'tok indentation_stack = ('tok * int) list


let pop_to_top stack =
  ([],0)

let rec pop_to kwd stack =
  match stack with
  | [] -> ([],0)
  | (kwd',indent) :: stack when kwd' = kwd -> stack, indent
  | _ :: stack -> pop_to kwd stack

let rec pop_to_kwds kwd_end_recursion = fun kwds stack ->
  match stack with
  | [] -> ([], (kwd_end_recursion, 0))
  | (kwd,indent) :: stack when List.memq kwd kwds -> stack, (kwd, indent)
  | _ :: stack -> pop_to_kwds kwd_end_recursion kwds stack

let fix indent eols indents =
  match eols with
  | [] -> indents
  | _ -> 
      match indents with
      | (pindent,peols) :: tail when pindent = indent -> 
          (indent, eols @ peols) :: tail
      | _ ->  (indent,eols) :: indents

(*****************************************************************************)
(* Debugging *)
(*****************************************************************************)

let _print_indentations list =
  print_string "Indentations :"; 
  print_newline ();
  List.iter (fun (indent, list) ->
      List.iter (fun pos -> 
          Printf.printf "Line at %d with %d" pos indent
      ) list
  ) list;
  print_newline ()

let _print_stack tokens stack =
  print_string "Indentation stack:"; 
  print_newline ();
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

(*
let print_exc e s =
  Printf.printf "Caught exception %s in %s" (Printexc.to_string e) s;
  print_newline ()
*)

(*****************************************************************************)
(* Helpers of helper *)
(*****************************************************************************)

(* less: maybe can get rid of this *)
let compute_indentations get_indentations lexing = 
 fun buf start_point end_point ->
  let text = buf.buf_text in
  Text.with_dup_point text start_point (fun curseur ->
    let lexbuf = lexing text curseur end_point in
    get_indentations (Text.get_position text start_point) lexbuf
  )

let rec pop_indentations indents =
  match indents with
  | [] -> raise Not_found
  | (indent, eols) :: indents ->
      match eols with
      | [] -> pop_indentations indents
      | eol :: eols -> (indent, eol, (indent,eols) :: indents)

let find_phrase_start start_regexp = fun buf curseur ->
  let text = buf.buf_text in
  try
    let _ = Text.search_backward text start_regexp curseur in ()
  with
    Not_found -> Text.set_position text curseur 0

(*s: function [[Simple.set_indent]] *)
(* modify the indentation of (point) line. Does not modify point *)
let set_indent text point offset = 
  Text.with_dup_point text point (fun curseur ->
    Text.bmove text curseur (Text.point_to_bol text curseur);

    let rec iter offset =
      let c = Text.get_char text curseur in
      if offset > 0 then
        if c = ' ' then
          (Text.fmove text curseur 1; iter (offset - 1))
        else
        if c = '\t' then
          (Text.delete text curseur 1;
           iter offset)
        else
          (Text.insert text curseur (String.make offset ' '))
      else
      if c = ' ' || c='\t' then
        (Text.delete text curseur 1;
          iter 0)
    in
    iter offset
  )
(*e: function [[Simple.set_indent]] *)

(*****************************************************************************)
(* Indent between points (for indent_func) helpers using get_indentations *)
(*****************************************************************************)

let indent_between_points get_indentations lexing start_regexp = 
 fun buf start_point end_point ->
  let text = buf.buf_text in
  text |> Text.with_session (fun session ->
  Text.with_dup_point text start_point (fun curseur ->
   try
    find_phrase_start start_regexp buf curseur;
    let indentations = 
      compute_indentations get_indentations lexing buf curseur end_point in
    (* remove the Eof indentation *)
    let _,_,indentations = pop_indentations indentations in
    (* indent other lines *)
    let rec iter indents =
      let (current,pos,indents) = pop_indentations indents in
      Text.set_position text curseur (pos+1);
      set_indent text curseur current;
      iter indents
    in
    iter indentations
  with Not_found -> ()
 ))

(*****************************************************************************)
(* Indent current line (Tab) helper *)
(*****************************************************************************)
let indent_current_line get_indentations lexing start_regexp color_region = 
 fun frame ->
  let (buf, text, point) = Frame.buf_text_point frame in

  (* colorize line *)
  Text.with_dup_point text point (fun start_point ->
  Text.with_dup_point text point (fun end_point ->
    Text.bmove text start_point (Text.point_to_bol text start_point);
    Text.fmove text end_point (Text.point_to_eol text end_point);
    color_region buf start_point end_point;
  ));

  (* indentation *)
  Text.with_dup_point text point (fun curseur ->
    find_phrase_start start_regexp buf curseur;
    let indentations = 
      compute_indentations get_indentations lexing buf curseur point in
    let (_next,pos,tail) = pop_indentations indentations in
    let current =
      try
        let (current, _, _ ) = pop_indentations tail in 
        current
      with Not_found  -> 0
    in
    set_indent text point current
  )

(*****************************************************************************)
(* Electric return to indent? *)
(*****************************************************************************)

(* Interactive: indent the current line, insert newline and indent next line *)
(*
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
    let (next,pos,tail) = pop_indentations indentations in
    let current =
      try
        let (current, _, _ ) = pop_indentations tail in current
      with
        Not_found  -> 0
    in
    let session = Text.start_session text in
    set_indent text point current;
    Edit.insert_char frame '\n';
    set_indent text point next;
    Text.commit_session text session;
    Text.fmove text point next; 
    ()
  with
    e -> 
      Text.remove_point text curseur;
      Edit.insert_char frame '\n'
*)

(*e: features/indent.ml *)
