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


open Efuns
open Text
open Frame
open Top_window
  

let cut_frame frame =
  let window = frame.frm_window in
  if window.win_height > 3 then
    let h = window.win_height / 2 in
    let w1 = Window.create false
        (Window window) window.win_xpos window.win_ypos
        window.win_width h in
    let w2 = Window.create false (Window window) window.win_xpos 
        (window.win_ypos + h) 
      window.win_width (window.win_height - h) in
    window.win_down <- VComb (w1,w2);
    Frame.install w1 frame;
    w2 
  else
    window

let remove_frame frame =
  if frame.frm_mini_buffer = None then
    let window = frame.frm_window in
    match window.win_up with
      TopWindow _ -> ()
    | Window upwin ->
        Window.prev (Frame.install upwin) window

let v_cut_frame frame =
  if frame.frm_mini_buffer = None then
    let _ = Frame.create (cut_frame frame) None frame.frm_buffer in ()

let h_cut_frame frame =
  if frame.frm_mini_buffer = None then
    let window = frame.frm_window in
    if window.win_width > 10 then
      let wi = window.win_width / 2 in
      let w1 = Window.create false
          (Window window) window.win_xpos window.win_ypos
          wi window.win_height in
      let w2 = Window.create false (Window window) (window.win_xpos + wi) 
        window.win_ypos
          (window.win_width - wi) window.win_height in
      window.win_down <- HComb (w1,w2);
      Frame.install w1 frame;
      let _ = Frame.create w2 None frame.frm_buffer in ()

let delete_frame frame =
  if frame.frm_mini_buffer = None then
    let window = frame.frm_window in
    match window.win_up with
      TopWindow _ -> ()
    | Window upwin ->
        Frame.install upwin frame;
        Frame.active frame

let one_frame frame =
  if frame.frm_mini_buffer = None then
    let window = frame.frm_window in
    let top_window = Window.top window in
    if not (top_window.top_windows == window) then
      begin
        Frame.install top_window.top_windows frame;
        Frame.active frame
      end
      
      
let next_frame frame =
  let window = frame.frm_window in
  Window.next Frame.active window
