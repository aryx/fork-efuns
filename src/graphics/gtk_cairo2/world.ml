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

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type world = {
  (* the "model" (but actually accessible also via Globals.location()) *)
  edt: Efuns.editor;

  (* dimensions *)
  metrics: layout;

  (* first cairo layer, for heavy computation e.g. the minimap *)
  mutable base: Cairo.Surface.t;
  (* second cairo layer, for scrolling window on minimap *)
  mutable overlay: Cairo.Surface.t;
  (* the final drawing area *)
  mutable final: Cairo.Surface.t;

  (* pango is better than the (simpler but buggy) toy text api in cairo *)
  ly: Pango.layout;

  (* we redraw (expensive) the minimap only if this triple changes:
   *  (buf_name, version of text, page)
   *)
  mutable last_top_frame_info:(string * Text.version * int);
}

(* stuff are set as mutable because they are derived from the other fields *)
and layout = {
  font_width: float;
  font_height: float;

  main_width: float; (* should be loc.loc_width * font_width *)
  main_height: float; (* should be loc.loc_height * font_height *)

  mini_factor: float;
  mutable mini_width: float;
  mutable linemax: int; (* number of lines the minimap can display in a "page"*)

  margin_factor: float;
  mutable margin_width: float;

  (* main + mini _ margin *)
  mutable full_width: int;
  mutable full_height: int;
}
