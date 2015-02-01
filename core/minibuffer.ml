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

open Text
open Efuns
open Frame
open Ebuffer
open Top_window
open Simple


let charreprs = Array.init 256 (fun i ->   String.make 1 (Char.chr i))
let _ =
  charreprs.(9) <- String.make !tab_size ' '

let buf_create location text local_map =
  let buf =
    { 
      buf_modified = 0;
      buf_text = text;
      buf_name = "*Minibuffer*";
      buf_filename = None;
      buf_last_saved = version text;
      buf_history = [];
      buf_charreprs = charreprs;
      buf_map = local_map;
      buf_map_partial = true;
      buf_point = Text.add_point text;
      buf_sync = false;
      buf_mark = None;
      buf_start = Text.add_point text;
      buf_shared = 0;
      buf_syntax_table = Ebuffer.default_syntax_table;
      buf_finalizers = [];
      buf_major_mode = fondamental_mode;
      buf_minor_modes = [];
      buf_vars = Local.vars ();
      buf_location = location;
    } in
  buf


let kill mini_frame old_frame =
  let window = mini_frame.frm_window in
  let top_window = Window.top window in
  let location = top_window.top_location in
  clear_message top_window;
  top_window.top_mini_buffers <- List.tl top_window.top_mini_buffers;
  if old_frame.frm_killed then
    Frame.unkill window old_frame;
  top_window.top_active_frame <- old_frame;
  Frame.kill mini_frame

let return action old_frame mini_frame =
  let repstr = Text.to_string mini_frame.frm_buffer.buf_text in
  kill mini_frame old_frame;
  action old_frame repstr

let create frame local_map request =
  let window = frame.frm_window in
  let top_window = Window.top window in
  let location = top_window.top_location in
  let mini_text = Text.create "" in
  let qlen = String.length request in
  let request = if qlen < 50 then request else
      (String.sub request 0 47 ^ "...") in
  let mini_buf = buf_create location mini_text local_map in
  let mini_window = Window.create true (TopWindow top_window) 
    qlen (top_window.top_height - 1)
    (top_window.top_width - qlen) 1 in
  let mini_frame = Frame.create mini_window (Some request) mini_buf
  in    
  mini_frame.frm_cutline <- max_int;
  mini_frame.frm_has_status_line <- 0;
  top_window.top_mini_buffers <- mini_frame :: top_window.top_mini_buffers;
  Keymap.add_binding local_map [ControlMap, Char.code 'g']
    (fun mini_frame -> 
      kill mini_frame frame);
  mini_frame

let create_return frame local_map request default action =
  let mini_frame = create frame local_map request in
  insert_string mini_frame default;
  Keymap.add_binding local_map [NormalMap, XK.xk_Return] 
    (return action frame);
  mini_frame

let update_request frame request =
  let qlen = String.length request in
  let window = frame.frm_window in
  let top_window = Window.top window in
  let mini_window = Window.create true window.win_up
      qlen window.win_ypos
      (top_window.top_width - qlen) 1
  in
  frame.frm_width <- top_window.top_width - qlen;
  frame.frm_xpos <- qlen;
  frame.frm_mini_buffer <- Some request;
  frame.frm_redraw <- true
  