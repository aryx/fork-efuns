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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Main indent actions.
 *
 * See also prog_modes/common_indenter.ml for the functions to
 * help build indenters. Those indenters will then probably
 * be assigned to indent_func below, which will then allow to
 * use M-x indent_xxx functions to indent files.
 *
 *)

(*****************************************************************************)
(* Variable *)
(*****************************************************************************)

(* pad: I factorized some indent code by using a variable *)
let indent_func = Store.create_abstr "Indent.indent_func"

let get_indent_func buf =
  Var.get_var buf indent_func

(*****************************************************************************)
(* Indent actions *)
(*****************************************************************************)

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
(* Indent helpers *)
(*****************************************************************************)
(* This could go in prog_modes/common_indenter.ml, but this is also
 * used by other indenting strategies like ocaml_ocp_indent.ml
 *)

(*s: function [[Simple.set_indent]] *)
(* Modify the indentation of line of point,
 * untabify the line, and move the point to the indented level.
 *)
let set_indent text curseur offset = 
    Text.bmove text curseur (Text.point_to_bol text curseur);

    let rec iter offset =
      let c = Text.get_char text curseur in
      if offset > 0 then
        (match c with
        | ' ' ->
          Text.fmove text curseur 1; 
          iter (offset - 1)
        | '\t' ->
          Text.delete text curseur 1;
          iter offset
        | _ ->
          Text.insert text curseur (String.make offset ' ');
          Text.fmove text curseur offset
        )
      else
      if c = ' ' || c='\t' then begin
        Text.delete text curseur 1;
        iter 0;
      end            
    in
    iter offset
(*e: function [[Simple.set_indent]] *)

(*e: features/indent.ml *)
