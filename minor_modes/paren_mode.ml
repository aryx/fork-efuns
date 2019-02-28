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

module H = Highlight

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A simple matching-parenthesis highlighter.
 * 
 * This is really useful when editing code, especially Lisp code :)
 * Note that this minor mode highlights not only matching '(' ')' but also
 * matching '{' '}' and '[' ']'.
 *
 * todo:
 *  - highlight also when hover a parenthesis, not just when you type it.
 * less:
 *  - let each major mode specifies what is a parenthesis?
 *    ('<'? '[|'?, ...)
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

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

exception Exit
(*s: function [[Simple.highlight_paren]] *)
let highlight_paren frame =
  let (buf, text, point) = Frame.buf_text_point frame in

  (*s: [[Simple.highlight_paren()]] special code for HTML modes *)
  htmlp := (!Top_window.keypressed = Char.code '>');
  (*e: [[Simple.highlight_paren()]] special code for HTML modes *)

  (* bugfix: I was using Text.with_dup_point but we must store cursor
   * in highlighted_chars, which then was causing some out_of_bound exn
   * when exciting efuns, and also some weird display bugs,
   * because remove_point was putting some -1 in cursor!
   *)
  let cursor = Text.dup_point text point in
  try
    (* no previous char (this should never happen when called from
     * find_matching) *)
    if Text.bmove_res text cursor 1 = 0 
    then raise Exit;
  
    let c = Text.get_char text cursor in
    (* right now we highlight only when you type a closing paren *)
    if not (is_paren_end c) 
    then raise Exit;
  
    let rec iter stack =
      if Text.bmove_res text cursor 1 = 0 
      then begin 
        Top_window.mini_message frame "No matching parenthesis";
        raise Exit
      end;
  
      let d = Text.get_char text cursor in
      if is_paren_end d 
      then iter (d :: stack)
      else
        if is_paren_begin d 
        then
          match stack with
          | [] -> (* found matching par *)
            let attr = Text.get_attr text cursor in
            (*s: [[Simple.highlight_paren()]] remember highlighted chars *)
            H.highlighted_chars := (buf,cursor,attr) :: !H.highlighted_chars;
            (*e: [[Simple.highlight_paren()]] remember highlighted chars *)
            Text.set_attr text cursor (attr lor Text.highlight_bit);
            buf.buf_modified <- buf.buf_modified + 1
          | _c :: stack -> (* don't try to match *)
            (* we could check they are matching chars (like '{' with '}') *)
            iter stack
        else iter stack
    in
    iter []
  with Exit ->
   Text.remove_point text cursor
(*e: function [[Simple.highlight_paren]] *)

(*s: function [[Paren_mode.find_matching]] *)
let find_matching frame = 
  Edit.self_insert_command frame; 
  highlight_paren frame
(*e: function [[Paren_mode.find_matching]] *)

(*****************************************************************************)
(* The mode *)
(*****************************************************************************)

(*s: constant [[Paren_mode.mode]] *)
let mode = Ebuffer.new_minor_mode "paren" []
(*e: constant [[Paren_mode.mode]] *)

(*s: function [[Paren_mode.paren_mode]] *)
let paren_mode = 
  Minor_modes.toggle_minor mode
[@@interactive]
(*e: function [[Paren_mode.paren_mode]] *)

(*****************************************************************************)
(* Setup *)
(*****************************************************************************)
  
(*s: toplevel [[Paren_mode._1]] *)
let _ = 
  (* alt: we could use a major mode var that specifies what is a parenthesis *)
  [')'; '}'; ']'] |> List.iter (fun key -> 
    Keymap.add_minor_key mode [NormalMap, Char.code key] find_matching
  )
(*e: toplevel [[Paren_mode._1]] *)

(*e: minor_modes/paren_mode.ml *)
