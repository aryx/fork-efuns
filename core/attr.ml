(*s: core/attr.ml *)
(*s: copyright header efuns *)
(***********************************************************************)
(*                                                                     *)
(*                             Efuns                                   *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
(*e: copyright header efuns *)
open Efuns

(* see also Text.make_attr *)

(*s: function [[Window.get_font]] *)
let get_font font_name =
  let edt = Globals.editor() in
  try
    Hashtbl.find edt.loc_fonts font_name
  with Not_found ->
    if edt.loc_fonts_n = 256 
    then raise Not_found
    else begin
      let n = edt.loc_fonts_n in
      edt.loc_fonts_n <- n + 1;
      edt.loc_fonts_names.(n) <- font_name;
      Hashtbl.add edt.loc_fonts font_name n;
      n
    end
(*e: function [[Window.get_font]] *)
        
(*s: function [[Window.get_color]] *)
let get_color color_name =
  let edt = Globals.editor() in
  try
    Hashtbl.find edt.loc_colors color_name
  with Not_found ->
    if edt.loc_colors_n = 256 
    then raise Not_found
    else begin
      let n = edt.loc_colors_n in
      edt.loc_colors_n <- n + 1;
      edt.loc_colors_names.(n) <- color_name;
      Hashtbl.add edt.loc_colors color_name n;
      n
    end
(*e: function [[Window.get_color]] *)

(*e: core/attr.ml *)
