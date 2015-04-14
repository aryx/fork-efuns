(*s: minor_modes/paren_mode.ml *)
(*s: copyright header2 *)
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
(*e: copyright header2 *)
open Simple
open Efuns

module H = Highlight

(*s: constant Simple.htmlp *)
let htmlp = ref false
(*e: constant Simple.htmlp *)
(*s: function Simple.is_paren_end *)
let is_paren_end c = (c == '}') || (c == ']') || (c == ')')
  ||  (!htmlp && c == '>')
(*e: function Simple.is_paren_end *)
(*s: function Simple.is_paren_begin *)
let is_paren_begin c = (c == '{') || (c == '[') || (c == '(')
  ||  (!htmlp && c == '<')
(*e: function Simple.is_paren_begin *)

(*s: function Simple.highlight_paren *)
let highlight_paren frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  htmlp := (!Top_window.keypressed = Char.code '>');

  Text.with_dup_point text point (fun curseur ->
  if Text.bmove_res text curseur 1 = 0 
  then ()
  else
  let c = Text.get_char text curseur in
  if not (is_paren_end c) 
  then ()
  else
  let rec iter stack =
    if Text.bmove_res text curseur 1 = 0 
    then Top_window.mini_message frame "No matching parenthesis"
    else
    let d = Text.get_char text curseur in
    if is_paren_end d 
    then iter (d :: stack)
    else
    if is_paren_begin d 
    then
      match stack with
      | [] -> (* found matching par *)
          let attr = Text.get_attr text curseur in
          H.highlighted_chars := (buf,curseur,attr) :: !H.highlighted_chars;
          Text.set_attr text curseur (attr lor H.highlight_bit);
          buf.buf_modified <- buf.buf_modified + 1
      | _ :: stack -> (* don't try to match *)
          iter stack
    else iter stack
  in
  iter []
  )
(*e: function Simple.highlight_paren *)




(*s: constant Paren_mode.mode *)
let mode = Ebuffer.new_minor_mode "paren" []
(*e: constant Paren_mode.mode *)

(*s: function Paren_mode.find_matching *)
let find_matching frame = 
  self_insert_command frame; 
  highlight_paren frame
(*e: function Paren_mode.find_matching *)
  
(*s: toplevel Paren_mode._1 *)
let _ = 
  [ ')'; '}'; ']' ] |> List.iter (fun key -> 
    Keymap.add_binding mode.min_map [NormalMap, Char.code key] find_matching
  ) 
(*e: toplevel Paren_mode._1 *)

(*s: toplevel Paren_mode._2 *)
let _ = 
  Action.define_buffer_action "paren_mode" 
    (fun buf -> 
      if Ebuffer.modep buf mode 
      then Ebuffer.del_minor_mode buf mode
      else Ebuffer.set_minor_mode buf mode
    )
(*e: toplevel Paren_mode._2 *)
(*e: minor_modes/paren_mode.ml *)
