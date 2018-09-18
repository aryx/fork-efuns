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

(* pad: I factorize some indent code by using a variable *)
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

type indentations = (int * (Text.position list)) list

let rec pop_to_top stack =
  match stack with
    [] -> ([],0)
  | _ :: stack -> pop_to_top stack

let rec pop_to kwd stack =
  match stack with
    [] -> ([],0)
  | (kwd',indent) :: stack when kwd' = kwd -> stack, indent
  | _ :: stack -> pop_to kwd stack

let rec pop_to_kwds kwd_end_recursion = fun kwds stack ->
  match stack with
    [] -> ([], kwd_end_recursion, 0)
  | (kwd,indent) :: stack when List.memq kwd kwds -> 
      stack, kwd, indent
  | _ :: stack -> pop_to_kwds kwd_end_recursion kwds stack

let fix indent eols indents =
  match eols with
    [] -> indents
  | _ -> 
      match indents with
        (pindent,peols) :: tail when pindent = indent ->
          (indent, eols @ peols) :: tail
      | _ ->  (indent,eols) :: indents

let rec pop_indentations indents =
  match indents with
    [] -> raise Not_found
  | (indent, eols) :: indents ->
      match eols with
        [] -> pop_indentations indents
      | eol :: eols ->
          (indent, eol, (indent,eols) :: indents)


(* debugging *)
let print_indentations list =
  print_string "Indentations :"; 
  print_newline ();
  List.iter (fun (indent, list) ->
      List.iter (fun pos -> 
          Printf.printf "Line at %d with %d" pos indent
      ) list
  ) list;
  print_newline ()

type 'tok indentation_stack = ('tok * int) list

let print_stack tokens stack =
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

(*e: features/indent.ml *)
