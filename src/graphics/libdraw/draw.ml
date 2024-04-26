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

(* may raise exn? *)
external initdraw : string option -> string -> unit = "caml_draw_initdraw"

(* tests *)
external set_color : int -> int -> int -> int -> unit = "caml_draw_set_color"
external line: int -> int -> int -> int -> unit = "caml_draw_line"
external string: int -> int -> string -> unit = "caml_draw_string"

(* efuns specifics *)

external set_bg_color: int -> int -> int -> int -> unit = 
    "caml_draw_set_bg_color"
external clear_eol: int -> int -> int -> unit = "caml_draw_clear_eol"

external set_fg_color: int -> int -> int -> int -> unit = 
    "caml_draw_set_fg_color"
external draw_string: int -> int -> string -> unit = "caml_draw_draw_string"

external ekbd: unit -> int = "caml_draw_ekbd"
