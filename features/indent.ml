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

(*e: features/indent.ml *)
