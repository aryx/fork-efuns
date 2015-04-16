(* Yoann Padioleau
 *
 * Copyright (C) 2015 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common
open Efuns

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A poor's man outline mode.
 *   
 * I call it a poor's man outline because we actually generate
 * a new buffer. You can move, and when you switch back
 * it goes back to the previous buffer at the adjusted place.
 * But you can't really copy paste, move things around, or
 * even edit the outline headers.
 * But it's good enough for moving around things quickly at least!
 * It's a good first step.
 *  
 * Called outline_mode.ml so no conflict with pfff/h_files_format/outline.ml
 * 
 * 
 * todo:
 *  - do a regexp-based outliner
 *    let regexp = Str.regexp "\\(\\input\\|\\section\\|\\subsection\\|\\subsubsection\\|\\chapter\\)[*]?{\\([^}]+\\)}"
 *    look efuns_textbrowser
 * 
 * conventions: I use orig/origin and outl/outline to talk about the original
 * buffer and outl for the outline buffer.
 *)

(*****************************************************************************)
(* Types, constants, globals *)
(*****************************************************************************)

type level = int

(* sorted outline points in the original buffers *)
type outline_points = (level * Text.point) list

(* to go back from the outline buffer to positions in the original buffer *)
type outline_origin_points =
    string (* original buf_name *) * Text.point array (* line number -> point *)

let (outline_var: outline_points Local.var) = 
  Local.create_abstr "outline_var"

let (outline_origin: outline_origin_points Local.var) = 
  Local.create_abstr "outline_origin"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let copy_line_and_attr text cursor text2 =
  let len = Text.point_to_eol text cursor in
  let str = Text.sub text cursor len in
  Text.with_new_point text2 (fun cursor2 ->
    (* go to end *)
    Text.set_position text2 cursor2 (Text.size text2);

    Text.insert_at_end text2 str;
    (* now set the attr of this newly inserted string *)
    Text.iter text cursor len (fun cursor ->
      let attr = Text.get_attr text cursor in
      Text.set_attr text2 cursor2 attr;
      Text.fmove text2 cursor2 1;
    )
  );
  Text.insert_at_end text2 "...\n"


(*****************************************************************************)
(* Install *)
(*****************************************************************************)
let install _buf =
  (* less: set to read-only *)
  ()

let mode_name = "Outline"
let mode =  Ebuffer.new_major_mode mode_name [install]

(* less: we could also have a minor mode? *)

let create_outline_buffer lvl line_orig buf_name buf_orig =
  let new_text = Text.create "" in
  let buf = Ebuffer.create buf_name None new_text (Keymap.create ()) in
  Ebuffer.set_major_mode buf mode;

  (* so can go back from outline location to origin location *)
  let origin_points = ref [] in
  (* so go to the corresponding line in the newly created outline buffer *)
  let current_line_to_go_in_outline_buffer = ref 0 in

  let xs = Var.get_var buf_orig outline_var in
  let text = buf_orig.buf_text in

  (* if outline level request is 2, then keep lvl1, lvl2, but not lvl3 *)
  let xs = xs |> List.filter (fun (lvlx, _) -> lvlx <= lvl) in

  let current_line_in_outline = ref 0 in
  Text.with_new_point text (fun cursor ->
    xs +> List.iter (fun (_lvl, pt) ->
      Text.goto_point text cursor pt;
      Text.bmove text cursor (Text.point_to_bol text cursor);
      Common.push (Text.dup_point text cursor) origin_points;
      copy_line_and_attr text cursor new_text;

      let line_outline_pt = Text.point_line text pt in
      if line_orig >= line_outline_pt
      then current_line_to_go_in_outline_buffer := !current_line_in_outline;

      incr current_line_in_outline;
    )
  );
  (* set buf_point to correspond to line you were in originally *)
  Text.goto_line new_text buf.buf_point !current_line_to_go_in_outline_buffer;

  Var.set_local buf outline_origin 
    (buf_orig.buf_name, Array.of_list (List.rev !origin_points));

  buf

let outline_num frame =
  let lvl = !Top_window.keypressed - Char.code '0' in
  if not (lvl >= 0 || lvl <= 10)
  then failwith (spf "outline level is out of range: %d" lvl);

  let buf = frame.frm_buffer in
  if buf.buf_major_mode.maj_name = mode_name
  then begin
    let buf_outl = buf in
    let (bufname_orig, points_orig) = Var.get_var buf_outl outline_origin in
    let text_outl = buf_outl.buf_text in
    let line_outl = Text.point_line text_outl frame.frm_point in
    let pt_orig = points_orig.(line_outl) in
    Ebuffer.find_buffer_opt bufname_orig |> Common.do_option (fun buf_orig ->
      let text_orig = buf_orig.buf_text in
      Text.goto_point text_orig buf_orig.buf_point pt_orig;
      Frame.change_buffer frame.frm_window bufname_orig;
      (* after change_buffer the old frame should have been killed
       * and so the outline buffer should have no more references to it,
       * except if the user did a split window in which case
       * we can't gc it
       *)
      if buf_outl.buf_shared = 0 
      then begin 
        Ebuffer.kill buf_outl;
        points_orig |> Array.iter (fun pt -> Text.remove_point text_orig pt);
      end;
    )
  end
  else begin
    let buf_orig = buf in
    let bufname_outl = spf "*Outline-%d-%s*" lvl buf_orig.buf_name in
    let line_orig = Text.point_line buf_orig.buf_text frame.frm_point in
    let buf_outl = create_outline_buffer lvl line_orig bufname_outl buf_orig in
    Frame.change_buffer frame.frm_window buf_outl.buf_name
  end


(*****************************************************************************)
(* Setup *)
(*****************************************************************************)

let _ = 
  Hook.add_start_hook (fun () ->
    Action.define_action "outline_num" outline_num;

    Keymap.add_global_key [ControlMetaMap, Char.code '1'] 
      "outline_num" outline_num;
    Keymap.add_global_key [ControlMetaMap, Char.code '2'] 
      "outline_num" outline_num;
    Keymap.add_global_key [ControlMetaMap, Char.code '3'] 
      "outline_num" outline_num;
    Keymap.add_global_key [ControlMetaMap, Char.code '4'] 
      "outline_num" outline_num;
    Keymap.add_global_key [ControlMetaMap, Char.code '5'] 
      "outline_num" outline_num;

  )
