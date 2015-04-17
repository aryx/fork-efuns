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

module E = Entity_code
module HC = Highlight_code
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Common code to the different pfff-based efuns programming language modes,
 * for coloring, and for outline.
 * 
 * todo: 
 *  - indentation, 
 *  - codegraph based navig, 
 *  - ?
 *)


(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* copy of pfff/code_map/parsing2.ml *)
type ('ast, 'token) for_helper = {
  parse: Common.filename -> ('ast * 'token list) list;
  highlight: tag_hook:(Parse_info.info -> HC.category -> unit) ->
             Highlight_code.highlighter_preferences -> 'ast * 'token list ->
             unit;
(*  info_of_tok:('token -> Parse_info.info); *)
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* dupe of pfff/code_map/draw_microlevel.ml *)
let color_of_categ categ =
  let attrs = Highlight_code.info_of_category categ in
  attrs +> Common.find_some (fun attr ->
    match attr with
    | `FOREGROUND s 
    | `BACKGROUND s (* todo: should really draw the background of the text *)
      -> 
        Some (s)
    | _ -> None
  )

(* dupe of pfff/code_map/style.ml#size_font_multiplier_of_categ *)
(* this is part of the attribute, so it must be an int between
 * 0 and 255. Right now 0 means default size.
 * This is used only for the minimap for now.
 *)
let size_of_categ categ =
  match categ with
  | HC.Entity (kind, HC.Def2 _) ->
    (match kind with
    | E.Field | E.Method | E.ClassConstant | E.Constructor -> 1
    | _ -> 3
    )

  | HC.CommentSection0 -> 5
  | HC.CommentSection1 -> 4
  | HC.CommentSection2 -> 2
  | HC.CommentSection3 -> 1
  | HC.CommentSection4 -> 1

  | _ -> 0

(* for outline, maybe could factorize with level_of_categ *)
let level_of_categ categ =
  match categ with
  | HC.CommentSection0 -> 0

  (* maybe module def should be 1 too *)
  | HC.Entity (kind, HC.Def2 _) -> 
    (match kind with
    | E.Field | E.Method | E.ClassConstant | E.Constructor -> 3
    | _ -> 2
    )

  | HC.CommentSection1 -> 1

  | HC.CommentSection2 -> 2
  | HC.CommentSection3 -> 2
  | HC.CommentSection4 -> 2

  | _ -> 0


let colorize_and_set_outlines funcs buf file =
  let xs = funcs.parse file in
  let prefs = Highlight_code.default_highlighter_preferences in
  let text = buf.buf_text in

  (* for outline *)
  let outline_points = ref [] in
  let hcovered_lines = Hashtbl.create 101 in

  Text.with_new_point text (fun cursor ->
    xs |> List.iter (fun x -> 
      x |> funcs.highlight ~tag_hook:(fun info categ->
        let color = color_of_categ categ in
        let fontsize = size_of_categ categ in
        let lvl = level_of_categ categ in
        
        let pos = PI.pos_of_info info in
        Text.set_position text cursor pos;
        let line = PI.line_of_info info in

        if lvl > 0 && not (Hashtbl.mem hcovered_lines line) then begin
          Hashtbl.add hcovered_lines line true;
          Common.push (lvl, Text.dup_point text cursor) outline_points;
        end;

        let attr = Text.make_attr (Attr.get_color color) 1 fontsize false in
        let str = PI.str_of_info info in
        let len = String.length str in
        Text.set_attrs text cursor len attr
      ) prefs 
    )
  );
  (* less: need to set a finalizer for the points stored in outline_points?
   * meh
   *)
  let outline_points = 
    match List.rev !outline_points with
    (* to avoid some index out of bounds create at least one point *)
    | [] -> [0, Text.new_point text]
    | xs -> 
      (* we have no guarantee in which order the hooks are called *)
      xs |> List.sort (fun (_, pt_a) (_, pt_b) -> compare pt_a pt_b)
  in
  Var.set_local buf Outline_mode.outline_var outline_points;
  ()
