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
open Efuns
open Simple

module H = Highlight

(*s: constant [[Simple.htmlp]] *)
let htmlp = ref false
(*e: constant [[Simple.htmlp]] *)
(*s: function [[Simple.is_paren_end]] *)
let is_paren_end c = (c == '}') || (c == ']') || (c == ')')
  (*s: [[Simple.is_paren_end()]] extra conditions *)
    ||  (!htmlp && c == '>')
  (*e: [[Simple.is_paren_end()]] extra conditions *)
(*e: function [[Simple.is_paren_end]] *)
(*s: function [[Simple.is_paren_begin]] *)
let is_paren_begin c = (c == '{') || (c == '[') || (c == '(')
  (*s: [[Simple.is_paren_begin()]] extra conditions *)
    ||  (!htmlp && c == '<')
  (*e: [[Simple.is_paren_begin()]] extra conditions *)

(*e: function [[Simple.is_paren_begin]] *)

(*s: function [[Simple.highlight_paren]] *)
let highlight_paren frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in

  (*s: [[Simple.highlight_paren()]] special code for HTML modes *)
  htmlp := (!Top_window.keypressed = Char.code '>');
  (*e: [[Simple.highlight_paren()]] special code for HTML modes *)

  Text.with_dup_point text point (fun curseur ->
  if Text.bmove_res text curseur 1 = 0 
  then ()
  else
  let c = Text.get_char text curseur in
  (* right now highlighting only when type a closing paren *)
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
          (*s: [[Simple.highlight_paren()]] remember highlighted chars *)
          H.highlighted_chars := (buf,curseur,attr) :: !H.highlighted_chars;
          (*e: [[Simple.highlight_paren()]] remember highlighted chars *)
          Text.set_attr text curseur (attr lor Text.highlight_bit);
          buf.buf_modified <- buf.buf_modified + 1
      | _ :: stack -> (* don't try to match *)
          iter stack
    else iter stack
  in
  iter []
  )
(*e: function [[Simple.highlight_paren]] *)


(*s: constant [[Paren_mode.mode]] *)
let mode = Ebuffer.new_minor_mode "paren" []
(*e: constant [[Paren_mode.mode]] *)

(*s: function [[Paren_mode.find_matching]] *)
let find_matching frame = 
  self_insert_command frame; 
  highlight_paren frame
(*e: function [[Paren_mode.find_matching]] *)
  
(*s: toplevel [[Paren_mode._1]] *)
let _ = 
  [ ')'; '}'; ']' ] |> List.iter (fun key -> 
    Keymap.add_binding mode.min_map [NormalMap, Char.code key] find_matching
  ) 
(*e: toplevel [[Paren_mode._1]] *)

(*s: toplevel [[Paren_mode._2]] *)
let _ = 
  Action.define_action "paren_mode" (Minor_modes.toggle_minor mode)
(*e: toplevel [[Paren_mode._2]] *)
(*e: minor_modes/paren_mode.ml *)
