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
(* Common code to the different pfff-based efuns modes
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
 *)
let size_of_categ categ =
  match categ with
  | HC.Entity (_kind, HC.Def2 _) -> 3

  | HC.CommentSection0 -> 6
  | HC.CommentSection1 -> 4
  | HC.CommentSection2 -> 2
  | HC.CommentSection3 -> 1
  | HC.CommentSection4 -> 1

  | _ -> 0


let colorize funcs buf file =
  let xs = funcs.parse file in
  let prefs = Highlight_code.default_highlighter_preferences in

  let text = buf.buf_text in
  let cursor = Text.new_point text in

  xs |> List.iter (fun x -> x |> funcs.highlight ~tag_hook:(fun info categ->
    let color = color_of_categ categ in
    let fontsize = size_of_categ categ in

    let pos = PI.pos_of_info info in
    Text.set_position text cursor pos;
    let attr = Text.make_attr (Attr.get_color color) 1 fontsize false in

    let str = PI.str_of_info info in
    let len = String.length str in
    Text.set_attrs text cursor len attr
  ) prefs 
  )
