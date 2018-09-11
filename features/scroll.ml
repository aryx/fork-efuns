(*s: features/scroll.ml *)
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
(* Screen *)
(*****************************************************************************)
  
(*s: function [[Simple.forward_screen]] *)
let forward_screen frame =
  frame.frm_force_start <- true;
  frame.frm_redraw <- true;
  frame.frm_y_offset <- frame.frm_y_offset + frame.frm_height - 2
(*e: function [[Simple.forward_screen]] *)

(*s: function [[Simple.backward_screen]] *)
let backward_screen frame =
  frame.frm_force_start <- true;
  frame.frm_redraw <- true;
  frame.frm_y_offset <- frame.frm_y_offset - frame.frm_height + 2
(*e: function [[Simple.backward_screen]] *)

(*****************************************************************************)
(* Scroll *)
(*****************************************************************************)

(*s: function [[Simple.scroll_line]] *)
let scroll_line frame n =
  frame.frm_force_start <- true;
  frame.frm_redraw <- true;
  frame.frm_y_offset <- frame.frm_y_offset + n
(*e: function [[Simple.scroll_line]] *)

(*s: function [[Simple.scroll_down]] *)
let scroll_down frame =
  scroll_line frame 1;
  forward_line frame
(*e: function [[Simple.scroll_down]] *)

(*s: function [[Simple.scroll_up]] *)
let scroll_up frame =
  scroll_line frame (-1);
  backward_line frame
(*e: function [[Simple.scroll_up]] *)

(*s: function [[Simple.scroll_other_up]] *)
let scroll_other_up frame =
  Window.next scroll_up frame.frm_window
(*e: function [[Simple.scroll_other_up]] *)

(*s: function [[Simple.scroll_other_down]] *)
let scroll_other_down frame =
  Window.next scroll_down frame.frm_window
(*e: function [[Simple.scroll_other_down]] *)

(*s: function [[Simple.recenter]] *)
let recenter frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  frame.frm_force_start <- true;
  frame.frm_redraw <- true;
  Text.goto_point text frame.frm_start frame.frm_point;
  frame.frm_y_offset <- - frame.frm_height/2
(*e: function [[Simple.recenter]] *)

(*e: features/scroll.ml *)
