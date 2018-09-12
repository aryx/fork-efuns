(*s: features/edit.ml *)
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
open Move

(*****************************************************************************)
(* Basic insertion *)
(*****************************************************************************)

(*s: function [[Simple.insert_string]] *)
let insert_string frame str =
  let text = frame.frm_buffer.buf_text in
  let point = frame.frm_point in
  Text.insert text point str;
  Text.fmove text point (String.length str)
(*e: function [[Simple.insert_string]] *)
  
(*s: constant [[Simple.single_char]] *)
let single_char = String.make 1 ' '
(*e: constant [[Simple.single_char]] *)
(*s: function [[Simple.insert_char]] *)
(* could factorize and just call insert_string? *)
let insert_char frame char =
  let text = frame.frm_buffer.buf_text in
  let point = frame.frm_point in
  single_char.[0] <- char;
  Text.insert text point single_char;
  Text.fmove text point 1
(*e: function [[Simple.insert_char]] *)

(*s: function [[Simple.insert_return]] *)
let insert_return frame =
  insert_char frame '\n'
[@@interactive]
(*e: function [[Simple.insert_return]] *)

(*s: function [[Simple.insert_at_place]] *)
let insert_at_place frame char =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let c = Text.get_char text point in
  if c = '\n' 
  then insert_char frame char
  else begin
    Text.delete text point 1;
    single_char.[0] <- char;    
    Text.insert text point single_char;
    Text.fmove text point 1
  end
(*e: function [[Simple.insert_at_place]] *)


(*s: constant [[Simple.overwrite_mode]] *)
let overwrite_mode = Ebuffer.new_minor_mode "Over" []
(*e: constant [[Simple.overwrite_mode]] *)
  
  
(*s: function [[Simple.self_insert_command]] *)
let self_insert_command frame =
  let char = Char.chr !Top_window.keypressed in
  let buf = frame.frm_buffer in
  (*s: [[Simple.self_insert_command()]] if overwrite mode *)
  if Ebuffer.has_minor_mode buf overwrite_mode 
  then insert_at_place frame char
  (*e: [[Simple.self_insert_command()]] if overwrite mode *)
  else insert_char frame char
(*e: function [[Simple.self_insert_command]] *)
    
(*s: function [[Simple.char_insert_command]] *)
let char_insert_command char frame =
  let buf = frame.frm_buffer in
  (*s: [[Simple.self_insert_command()]] if overwrite mode *)
  if Ebuffer.has_minor_mode buf overwrite_mode 
  then insert_at_place frame char
  (*e: [[Simple.self_insert_command()]] if overwrite mode *)
  else insert_char frame char
(*e: function [[Simple.char_insert_command]] *)

(*****************************************************************************)
(* Basic deletion *)
(*****************************************************************************)

(*s: function [[Simple.delete_char]] *)
let delete_char frame =
  let text = frame.frm_buffer.buf_text in
  Text.delete text frame.frm_point 1
[@@interactive]
(*e: function [[Simple.delete_char]] *)

(*s: function [[Simple.delete_backspace_char]] *)
let delete_backspace_char frame =
  let text = frame.frm_buffer.buf_text in
  if Text.bmove_res text frame.frm_point 1 <> 0 
  then Text.delete text frame.frm_point 1
[@@interactive]
(*e: function [[Simple.delete_backspace_char]] *)

(*****************************************************************************)
(* Words *)
(*****************************************************************************)

(*s: function [[Simple.delete_backward_word]] *)
let delete_backward_word buf point =
  let text = buf.buf_text in
  Text.with_dup_point text point (fun old_point ->
    backward_word buf point;
    Text.delete text point (Text.distance text point old_point)
  )
(*e: function [[Simple.delete_backward_word]] *)

(*s: function [[Simple.delete_forward_word]] *)
let delete_forward_word buf point =
  let text = buf.buf_text in
  Text.with_dup_point text point (fun old_point ->
    forward_word buf point;
    let len = Text.distance text old_point point in
    Text.bmove text point len;
    Text.delete text point len
  )
(*e: function [[Simple.delete_forward_word]] *)

(*s: function [[Simple.on_word]] *)
let on_word buf point f =
  let text = buf.buf_text in
  text |> Text.with_session (fun () ->
    let syntax = buf.buf_syntax_table in
    to_begin_of_word text point syntax;
    Text.with_dup_point text point (fun pos1 ->
      to_end_of_word text point syntax;
      let _,word1 = Text.delete_res text pos1 (Text.distance text pos1 point) in
      let w = f word1 in
      Text.insert text pos1 w;
      Text.fmove text point (String.length w);
    )
  )
(*e: function [[Simple.on_word]] *)
  
(*s: function [[Simple.transpose_words]] *)
let transpose_words buf point =
  let text = buf.buf_text in
  text |> Text.with_session (fun () ->
    let syntax = buf.buf_syntax_table in
    in_prev_word text point syntax;
    to_begin_of_word text point syntax;
    Text.with_dup_point text point (fun pos1 ->
      to_end_of_word text point syntax;
      let _,word1 = Text.delete_res text pos1 (Text.distance text pos1 point) in
      Text.goto_point text point pos1;
      in_next_word text point syntax;
      Text.with_dup_point text point (fun pos2 ->
        to_end_of_word text point syntax;
        let _,word2 = Text.delete_res text pos2 (Text.distance text pos2 point) in    
        Text.insert text pos1 word2;
        Text.insert text pos2 word1;
        Text.fmove text point (String.length word1);
  )))
(*e: function [[Simple.transpose_words]] *)

(*s: function [[Simple.transpose_chars]] *)
let transpose_chars buf point =
  let text = buf.buf_text in
  text |> Text.with_session (fun () ->
    Text.bmove text point 1;
    let pos,c1 = Text.delete_res text point 1 in
    Text.fmove text point 1;
    Text.insert text point c1;
  );
  Text.fmove text point 1;
  ()
(*e: function [[Simple.transpose_chars]] *)

(*****************************************************************************)
(* Overwrite *)
(*****************************************************************************)

(*s: function [[Edit.toggle_overwrite_mode]] *)
let toggle_overwrite_mode frm =
  let buf = frm.frm_buffer in
  let mode = overwrite_mode in
  if Ebuffer.has_minor_mode buf mode 
  then Ebuffer.del_minor_mode buf mode
  else Ebuffer.set_minor_mode buf mode
[@@interactive "overwrite_mode"]
(*e: function [[Edit.toggle_overwrite_mode]] *)
 

(*****************************************************************************)
(* Undo *)
(*****************************************************************************)

(*s: function [[Simple.undo]] *)
let undo frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in  
  let point = frame.frm_point in
  let action, at_point, len = Text.undo text in
  frame.frm_last_text_updated <- Text.version text - 1;
  (*s: save current pos from frame for position history navigation (in simple.ml) *)
  save_current_pos frame;
  (*e: save current pos from frame for position history navigation (in simple.ml) *)
  Text.set_position text point at_point;
  Text.fmove text point len
[@@interactive]
(*e: function [[Simple.undo]] *)

(*e: features/edit.ml *)
