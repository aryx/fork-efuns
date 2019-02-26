(*s: features/move.ml *)
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

(*****************************************************************************)
(* Basic navigation *)
(*****************************************************************************)

(*s: function [[Simple.move_backward]] *)
let move_backward frame delta =
  let (_buf, text, point) = Frame.buf_text_point frame in
  Text.bmove text point delta
(*e: function [[Simple.move_backward]] *)

(*s: function [[Simple.move_forward]] *)
let move_forward frame delta =
  let (_buf, text, point) = Frame.buf_text_point frame in
  Text.fmove text point delta
(*e: function [[Simple.move_forward]] *)

(*
let move_backward frame =
   Functions.move_backward frame 1; ()

let move_forward frame = 
  Functions.move_forward frame 1; () 
*) 

(*s: function [[Simple.begin_to_point]] *)
let begin_to_point frame =
  let (_, text, point) = Frame.buf_text_point frame in
  Text.point_to_bol text point
(*e: function [[Simple.begin_to_point]] *)

(*s: function [[Simple.point_to_end]] *)
let point_to_end frame =
  let (_, text, point) = Frame.buf_text_point frame in
  Text.point_to_eol text point
(*e: function [[Simple.point_to_end]] *)

(*****************************************************************************)
(* Words *)
(*****************************************************************************)

(*s: function [[Simple.in_next_word]] *)
let in_next_word text mark syntax =
  while (not syntax.(Char.code (Text.get_char text mark))) &&
        Text.fmove_res text mark 1 = 1 
  do () done
(*e: function [[Simple.in_next_word]] *)

(*s: function [[Simple.in_prev_word]] *)
let in_prev_word text mark syntax =
  while Text.bmove_res text mark 1 = 1 &&
        (not syntax.(Char.code (Text.get_char text mark)))
  do () done
(*e: function [[Simple.in_prev_word]] *)


(*s: function [[Simple.to_begin_of_word]] *)
let to_begin_of_word text mark syntax =
  if Text.bmove_res text mark 1 = 1 then
    begin
      while syntax.(Char.code (Text.get_char text mark)) &&
            (Text.bmove_res text mark 1) <> 0 
      do () done;
      if not syntax.(Char.code (Text.get_char text mark)) 
      then (Text.fmove text mark 1)
    end
(*e: function [[Simple.to_begin_of_word]] *)

(*s: function [[Simple.to_end_of_word]] *)
let to_end_of_word text mark syntax =
  while syntax.(Char.code (Text.get_char text mark)) &&
       (Text.fmove_res text mark 1) <> 0 
  do () done
(*e: function [[Simple.to_end_of_word]] *)


(*s: function [[Simple.backward_word]] *)
let backward_word buf point =
  let text = buf.buf_text in
  let syntax = buf.buf_syntax_table in
  in_prev_word text point syntax;
  to_begin_of_word text point syntax
(*e: function [[Simple.backward_word]] *)

(*s: function [[Simple.forward_word]] *)
let forward_word buf point =
  let text = buf.buf_text in
  let syntax = buf.buf_syntax_table in
  in_next_word text point syntax;
  to_end_of_word text point syntax
(*e: function [[Simple.forward_word]] *)

(*s: function [[Simple.beginning_of_word]] *)
let beginning_of_word buf point =
  let text = buf.buf_text in
  let syntax = buf.buf_syntax_table in
  Text.with_dup_point text point (fun mark ->
    to_begin_of_word text mark syntax;
    let s = Text.region text mark point in
    s
  )
(*e: function [[Simple.beginning_of_word]] *)

(*s: function [[Simple.end_of_word]] *)
let end_of_word  buf point =
  let text = buf.buf_text in
  let syntax = buf.buf_syntax_table in
  Text.with_dup_point text point (fun mark ->
    to_end_of_word text mark syntax;
    let s = Text.region text point mark in
    s
  )
(*e: function [[Simple.end_of_word]] *)

(*s: function [[Simple.current_word]] *)
let current_word buf point =
  (beginning_of_word buf point) ^ (end_of_word buf point)
(*e: function [[Simple.current_word]] *)

(*****************************************************************************)
(* Columns *)
(*****************************************************************************)

(*s: function [[Simple.line_size]] *)
let line_size frame =
  (point_to_end frame) + (point_to_end frame)
(*e: function [[Simple.line_size]] *)

(*s: function [[Simple.beginning_of_line]] *)
let beginning_of_line frame =
  move_backward frame (begin_to_point frame) |> ignore
[@@interactive]
(*e: function [[Simple.beginning_of_line]] *)

(*s: function [[Simple.end_of_line]] *)
let end_of_line frame =
  move_forward frame (point_to_end frame) |> ignore
[@@interactive]
(*e: function [[Simple.end_of_line]] *)

(*s: constant [[Simple.temporary_goal_column]] *)
let temporary_goal_column = 
  Store.create_abstr "Simple.temporary_goal_column"
(*e: constant [[Simple.temporary_goal_column]] *)

(*s: function [[Simple.goal_column]] *)
let rec goal_column frame =
  let cur_col = begin_to_point frame in
  if frame.frm_last_action == forward_line ||
     frame.frm_last_action == backward_line
  then 
    try Var.get_local frame.frm_buffer temporary_goal_column
    with Not_found -> cur_col
  else cur_col
(*e: function [[Simple.goal_column]] *)

(*s: function [[Simple.move_to_goal_column]] *)
and move_to_goal_column frame goal_col =
  move_backward frame (begin_to_point frame) |> ignore;
  move_forward frame (min goal_col (point_to_end frame)) |> ignore;
  Var.set_local frame.frm_buffer temporary_goal_column goal_col
(*e: function [[Simple.move_to_goal_column]] *)

(*****************************************************************************)
(* Lines *)
(*****************************************************************************)

(*s: function [[Simple.forward_line]] *)
and forward_line frame =
  let text = frame.frm_buffer.buf_text in
  let point = frame.frm_point in
  if Text.point_line text point < Text.nbr_lines text then begin
    let goal_col = goal_column frame in
    end_of_line frame;
    move_forward frame 1 |> ignore;
    move_to_goal_column frame goal_col;
  end
(*e: function [[Simple.forward_line]] *)

(*s: function [[Simple.backward_line]] *)
and backward_line frame =
  let text = frame.frm_buffer.buf_text in
  let point = frame.frm_point in
  if Text.point_line text point > 0 then begin
    let goal_col = goal_column frame in
    beginning_of_line frame;
    move_backward frame 1 |> ignore;
    move_to_goal_column frame goal_col;
  end
(*e: function [[Simple.backward_line]] *)

let forward_line = forward_line
[@@interactive]

let backward_line = backward_line
[@@interactive]

(*****************************************************************************)
(* Paragraphs *)
(*****************************************************************************)

(*s: function [[Simple.backward_paragraph]] *)
let backward_paragraph buf point =
  let text = buf.buf_text in
  while Text.bmove_res text point 1 = 1 && 
        (let c = Text.get_char text point in c = '\n' || c = ' ')
  do () done;
  try
    Text.search_backward text (Str.regexp "\n *\n") point |> ignore;
    Text.fmove text point 1
  with Not_found -> Text.set_position text point 0
(*e: function [[Simple.backward_paragraph]] *)

(*s: function [[Simple.forward_paragraph]] *)
let forward_paragraph buf point =
  let text = buf.buf_text in
  while (let c = Text.get_char text point in c = '\n' || c = ' ') &&
         Text.fmove_res text point 1 = 1 
  do () done;
  try
    Text.search_forward text (Str.regexp "\n *\n") point |> ignore;
    Text.fmove text point 1
  with Not_found -> Text.set_position text point (Text.size text)
(*e: function [[Simple.forward_paragraph]] *)

(*****************************************************************************)
(* Position history *)
(*****************************************************************************)

(*s: constant [[Simple.history_pos_max]] *)
let history_pos_max = 10
(*e: constant [[Simple.history_pos_max]] *)

(*s: function [[Simple.save_current_pos]] *)
let save_current_pos frame =
  let (buf, text, point) = Frame.buf_text_point frame in
  if Array.length buf.buf_history_pos < history_pos_max
  then buf.buf_history_pos <- Array.make history_pos_max None;
  let arr = buf.buf_history_pos in
  arr.(history_pos_max -1) |> Common.do_option (fun pt ->
     Text.remove_point text pt
  );
  Array.blit arr 0 arr 1 (history_pos_max - 1);
  arr.(0) <- Some (Text.dup_point buf.buf_text point)
(*e: function [[Simple.save_current_pos]] *)

(*s: function [[Simple.goto_last_saved_pos]] *)
let goto_last_saved_pos frame =
  let (buf, text, point) = Frame.buf_text_point frame in
  if Array.length buf.buf_history_pos < history_pos_max
  then buf.buf_history_pos <- Array.make history_pos_max None;
  let arr = buf.buf_history_pos in
  let head = arr.(0) in
  Array.blit arr 1 arr 0 (history_pos_max - 1);
  arr.(history_pos_max - 1) <- None;
  match head with
  | Some pt -> Text.goto_point text point pt
  | None -> failwith "No position history"
[@@interactive]
(*e: function [[Simple.goto_last_saved_pos]] *)

(*****************************************************************************)
(* File *)
(*****************************************************************************)

(*s: function [[Simple.end_of_file]] *)
let end_of_file frame =
  let (_, text, point) = Frame.buf_text_point frame in
  (*s: save current pos from frame for position history navigation (in simple.ml) *)
  save_current_pos frame;
  (*e: save current pos from frame for position history navigation (in simple.ml) *)
  Text.set_position text point (Text.size text)
[@@interactive]
(*e: function [[Simple.end_of_file]] *)

(*s: function [[Simple.begin_of_file]] *)
let begin_of_file frame =
  let (_, text, point) = Frame.buf_text_point frame in
  (*s: save current pos from frame for position history navigation (in simple.ml) *)
  save_current_pos frame;
  (*e: save current pos from frame for position history navigation (in simple.ml) *)
  Text.set_position text point 0
[@@interactive]
(*e: function [[Simple.begin_of_file]] *)

(*****************************************************************************)
(* Mark *)
(*****************************************************************************)

(*s: function [[Simple.point_at_mark]] *)
let point_at_mark frame =
  let (buf, text, point) = Frame.buf_text_point frame in
  let mark = Ebuffer.get_mark buf point in
  let pos = Text.get_position text point in
  Text.goto_point text point mark;
  Text.set_position text mark pos
[@@interactive]
(*e: function [[Simple.point_at_mark]] *)
(*e: features/move.ml *)
