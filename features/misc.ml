(*s: features/misc.ml *)
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
(* Misc *)
(*****************************************************************************)

(*s: constant [[Simple.line_comment]] *)
let line_comment = Store.create_abstr "Fill_mode.line_comment"
(*e: constant [[Simple.line_comment]] *)

(*s: function [[Simple.insert_special_char]] *)
let insert_special_char frame =
  let key = !Top_window.keypressed in
  let char = Char.chr key in
  if char >= 'a' && char <= 'z' 
  then Edit.insert_char frame (Char.chr (key - 97))
  else Edit.insert_char frame (Char.chr (key - 65))
(*e: function [[Simple.insert_special_char]] *)

(*****************************************************************************)
(* Complexe *)
(*****************************************************************************)

(*s: constant [[Complex.buf_mtime]] *)
let buf_mtime = Store.create "buf_mtime" string_of_float float_of_string
(*e: constant [[Complex.buf_mtime]] *)

(*s: function [[Complex.update_time]] *)
let update_time buf =
  try
    buf.buf_filename |> Common.do_option (fun file ->
      let st = Unix.lstat file in
      if st.Unix.st_kind = Unix.S_REG 
      then Var.set_local buf buf_mtime st.Unix.st_mtime;
    )
  with _ -> ()
(*e: function [[Complex.update_time]] *)
      

(*s: function [[Complex.reload]] *)
let reload frame = 
  let (buf, text, _) = Frame.buf_text_point frame in
  match buf.buf_filename with
    None -> ()
  | Some file ->
      let inc = open_in file in
      let s = Utils.read_string inc in
      close_in inc;
      let point = frame.frm_point in
      let pos = Text.get_position text point in
      Text.clear buf.buf_text;
      Text.insert_at_end text s;
      Text.set_position text point pos;
      List.iter (fun f -> f buf) buf.buf_major_mode.maj_hooks;
      List.iter (fun minor_mode -> List.iter
          (fun f -> f buf) minor_mode.min_hooks) buf.buf_minor_modes;
      Frame.status_modified frame false 
(*e: function [[Complex.reload]] *)
      
(*s: function [[Complex.check_file]] *)
let check_file frame =
  try
    let buf = frame.frm_buffer in
    buf.buf_filename |> Common.do_option (fun file ->
      let st = Unix.lstat file in
      if st.Unix.st_kind = Unix.S_REG then
        try
          let time = Var.get_local buf buf_mtime in
          Var.set_local buf buf_mtime st.Unix.st_mtime;
          if time <> st.Unix.st_mtime then
            (Select.select_yes_or_no frame 
               (Printf.sprintf "%s changed on disk; reload (y/n) ?" 
                    buf.buf_name) 
                (fun bool ->
                   if bool 
                   then reload frame 
                   else Frame.status_modified frame true
                 )) |> ignore
       with _ -> Var.set_local buf buf_mtime st.Unix.st_mtime
    )
  with _ -> ()
[@@interactive]
(*e: function [[Complex.check_file]] *)
    

(*s: function [[Complex.exit_efuns]] *)
let exit frame =
  let buffers = Utils.list_of_hash (Globals.editor()).edt_buffers in
  Multi_buffers.save_buffers_and_action frame buffers (fun _ -> 
    (* todo: have some exit hooks? *)
    raise (Common.UnixExit 0)
  )
[@@interactive]
(*e: function [[Complex.exit_efuns]] *)


(*s: function [[Complex.window_load_buffer]] *)
let window_load_buffer frame = 
  Select.select_file_from_pwd frame "Find file: " 
    (fun str -> 
      let top_window = Top_window.create ()
          (*(Window.display top_window)*)
      in
      Frame.load_file top_window.window str |> ignore
    )
[@@interactive]
(*e: function [[Complex.window_load_buffer]] *)

(*s: function [[Complex.window_change_buffer]] *)
let window_change_buffer frame =
  Multi_buffers.select_buffer frame "Switch to buffer in new frame: " 
    (Multi_buffers.get_previous_frame ())
    (fun name ->
      let top_window = Top_window.create ()
           (*"TODO_Display"*) 
      in
      Frame.change_buffer top_window.window name
  )
[@@interactive]
(*e: function [[Complex.window_change_buffer]] *)

(*s: function [[Complex.goto_line]] *)
let goto_line frame =
  Select.simple_select frame "goto-line:" (fun name ->
    let line = int_of_string name in
    (*s: save current pos from frame for position history navigation *)
    Move.save_current_pos frame;
    (*e: save current pos from frame for position history navigation *)
    Text.goto_line frame.frm_buffer.buf_text frame.frm_point (line - 1)
  )
[@@interactive]
(*e: function [[Complex.goto_line]] *)

(*s: function [[Complex.goto_char]] *)
let goto_char frame =
  Select.simple_select frame "goto-char:" (fun name ->
    let char = int_of_string name in
    Text.set_position frame.frm_buffer.buf_text frame.frm_point char
  )
[@@interactive]
(*e: function [[Complex.goto_char]] *)


(*s: function [[Complex.get_pos]] *)
let describe_position frame =
  Top_window.message 
    (Window.top frame.frm_window)
    (Printf.sprintf "Char position %d" 
       (Text.get_position frame.frm_buffer.buf_text frame.frm_point))
[@@interactive]
(*e: function [[Complex.get_pos]] *)

(*s: function [[Misc.cursor_position]] *)
let cursor_position frm =
  let text = frm.frm_buffer.buf_text in
  let point = frm.frm_point in
  let char = Text.get_char text point in
  let coord = Text.point_coord text point in
  Top_window.message 
    (Window.top frm.frm_window)
    (Printf.sprintf "Char: '%c' (%d, #o%o, #x%x) point=%d line=%d column=%d" 
       char 
       (Char.code char) (Char.code char) (Char.code char)
       (Text.get_position text point)
       (coord.Text.c_line + 1) 
       coord.Text.c_col
    )
[@@interactive]
(*e: function [[Misc.cursor_position]] *)


(*****************************************************************************)
(* Toplevel *)
(*****************************************************************************)

(*s: toplevel [[Simple._1]] *)
let _ =
  Hook.add_start_hook (fun () ->
    Var.set_global line_comment "";
    (* this needs to be done early, not in config.ml, because some
     * major modes may want to access it to add stuff in their
     * own local versions (e.g., in caml_mode.ml to recolorize the buffer
     *)
    Var.set_global Ebuffer.saved_buffer_hooks [update_time];

  )
(*e: toplevel [[Simple._1]] *)
  
(*e: features/misc.ml *)
