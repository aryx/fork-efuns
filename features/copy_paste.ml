(*s: features/copy_paste.ml *)
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
(* Kill *)
(*****************************************************************************)

(*s: constant [[Simple.kill_size]] *)
let kill_size = ref 0
(*e: constant [[Simple.kill_size]] *)
(*s: constant [[Simple.kill_max]] *)
let kill_max = 10
(*e: constant [[Simple.kill_max]] *)
(*s: constant [[Simple.kill_ring]] *)
let kill_ring = Array.make kill_max ""
(*e: constant [[Simple.kill_ring]] *)
(*s: constant [[Simple.last_kill]] *)
let last_kill = ref None
(*e: constant [[Simple.last_kill]] *)
(*s: constant [[Simple.last_insert]] *)
let last_insert = ref None
(*e: constant [[Simple.last_insert]] *)

(* alt: 
 * - have a mutable clipboard here in simple.ml (or global.ml)
 *   pro: does not require a frame so can be done in lowlevel API (kill_string)
 * - have the clipboard be part of the backend
 *   pro: clearer that backends in graphics/ need to setup this
 *)
let add_clipboard frame str =
  let top_window = Window.top frame.frm_window in
  let graphic = Efuns.backend top_window in
  graphic.Xdraw.set_clipboard (Some str)

let paste_clipboard frame =
  let (_, text, point) = Frame.buf_text_point frame in
  let top_window = Window.top frame.frm_window in
  let graphic = Efuns.backend top_window in
  let str =
    match graphic.Xdraw.get_clipboard () with
    | None | Some "" -> Message.message frame "Nothing in the clipboard"; ""
    | Some s -> 
      graphic.Xdraw.set_clipboard None;
      s
  in
  let pos, len =  Text.insert_res text point str in
  Text.fmove text point len; 
  last_insert := Some(frame,pos,0,len)
[@@interactive]


(*s: function [[Simple.kill_string]] *)
let kill_string str =
  Array.blit kill_ring 0 kill_ring 1 (kill_max - 1);
  incr kill_size;
  kill_ring.(0) <- str
(*e: function [[Simple.kill_string]] *)


(*s: function [[Simple.kill_text]] *)
let kill_text text point len =
  let point,str = Text.delete_res text point len in
  match !last_kill with
  | Some (oldtext,oldpoint) when oldpoint = point && oldtext == text ->
      kill_ring.(0) <- kill_ring.(0)^str
  | _ ->
      last_kill := Some (text,point);
      kill_string str
(*e: function [[Simple.kill_text]] *)

(*s: function [[Simple.kill_end_of_line]] *)
let kill_end_of_line frame =
  let (_, text, _) = Frame.buf_text_point frame in
  let eol = point_to_end frame in
  let len = 
    (* if already at eol then kill the newline *)
    if eol = 0 
    then 1 
    else eol 
  in
  kill_text text frame.frm_point len
[@@interactive]
(*e: function [[Simple.kill_end_of_line]] *)

(*s: function [[Simple.kill_eol]] *)
let kill_eol buf point =
  let text = buf.buf_text in
  let eol = Text.point_to_eol text point in
  let len =
    if eol = 0 then 1 else eol
  in
  kill_text text point len
(*e: function [[Simple.kill_eol]] *)

(*s: function [[Simple.kill_bol]] *)
let kill_bol buf point =
  let text = buf.buf_text in
  let len = Text.point_to_bol text point in
  if len > 0 then
    ( Text.bmove text point len;
      kill_text text point len)
(*e: function [[Simple.kill_bol]] *)

(*s: function [[Simple.insert_killed]] *)
let insert_killed frame =
  let (_, text, point) = Frame.buf_text_point frame in
(*
  let top_window = Window.top frame.frm_window in
  let graphic = Efuns.backend top_window in
  let str =
    match graphic.Xdraw.get_clipboard () with
    | None | Some "" -> kill_ring.(0)
    | Some s -> 
      graphic.Xdraw.set_clipboard None;
      s
  in
*)
  let str = kill_ring.(0) in

  let pos, len =  Text.insert_res text point str in
  Text.fmove text point len; 
  last_insert := Some(frame,pos,0,len)
[@@interactive]
(*e: function [[Simple.insert_killed]] *)

(*s: function [[Simple.insert_next_killed]] *)
let insert_next_killed frame =
  let (_, text, point) = Frame.buf_text_point frame in
  match !last_insert with
  |  Some (oldframe,oldpoint,n,len) when 
        oldframe == frame && oldpoint + len = Text.get_position text point ->
      let n = 
        if n = (min !kill_size kill_max) - 1 
        then 0 
        else n+1 
      in
      Text.bmove text point len;
      Text.delete text point len;
      let pos, len =  Text.insert_res text point kill_ring.(n) in
      Text.fmove text point len;
      last_insert := Some(frame,pos,n,len)
  | _ -> ()
[@@interactive]
(*e: function [[Simple.insert_next_killed]] *)


(*s: function [[Simple.kill_region]] *)
let kill_region frame =
  let (buf, text, point) = Frame.buf_text_point frame in
  let mark =
    match buf.buf_mark with
    | None -> failwith "No mark set"
    | Some mark -> mark
  in
  let (start,term) = 
    if mark > point then (point,mark) else (mark,point)
  in
  let _,region = Text.delete_res text start (Text.distance text start term) in
  (* less: would be better to do that in kill_string *)
  add_clipboard frame region;
  kill_string region
[@@interactive]
(*e: function [[Simple.kill_region]] *)

(*s: function [[Simple.copy_region]] *)
(* copy-region-as-kill-nomark in emacs *)
let copy_region frame =
  let (buf, text, point) = Frame.buf_text_point frame in
  let mark =
    match buf.buf_mark with
    | None -> failwith "No mark set"
    | Some mark -> 
        buf.buf_mark <- None;
        mark
  in
  let (start,term) = 
    if mark > point then (point,mark) else (mark,point)
  in
  let region = Text.sub text start (Text.distance text start term) in
  add_clipboard frame region;
  kill_string region;
  (* need a message because as opposed to a cut this is not directly visible *)
  Message.message frame "Region saved"
[@@interactive]
(*e: function [[Simple.copy_region]] *)



(*s: function [[Complex.mark_at_point]] *)
let mark_at_point frame =
  Ebuffer.set_mark frame.frm_buffer frame.frm_point;
  (*s: save current pos from frame for position history navigation *)
  Move.save_current_pos frame;
  (*e: save current pos from frame for position history navigation *)
  Message.message frame "Mark set";
  ()
[@@interactive]
(*e: function [[Complex.mark_at_point]] *)
(*e: features/copy_paste.ml *)
