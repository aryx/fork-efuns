(*s: core/window.ml *)
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

(*s: function Window.create_at_top *)
let create_at_top xpos ypos width height =
  let rec window = {
    win_xpos = xpos;
    win_ypos = ypos;
    win_width = width;
    win_height = height;

    win_down = NoFrame (); 
    win_up = Window window;

    win_mini = false;
  } in
  window
(*e: function Window.create_at_top *)

(*s: function Window.create *)
let create mini up_window xpos ypos width height =
  {
  win_xpos = xpos;
  win_ypos = ypos;

  win_width = width;
  win_height = height;

  win_down = NoFrame (); 
  win_up = up_window;

  win_mini = mini;
  }
(*e: function Window.create *)

(*s: function Window.top *)
let top window = 
  let rec iter window =
    match window.win_up with
      TopWindow top_window -> top_window
    | Window window -> iter window
  in
  iter window
(*e: function Window.top *)

(*s: function Window.iter *)
let iter f window = 
  let rec iter1 window =
    match window.win_down with
    | HComb (w1,w2) -> iter1 w1; iter1 w2       
    | VComb (w1,w2) -> iter1 w1; iter1 w2
    | WFrame frame -> f frame
    | NoFrame _ -> ()
  in
  iter1 window
(*e: function Window.iter *)

(*s: function Window.first *)
let rec first f window =
  match window.win_down with
    WFrame frame -> f frame
  | HComb (w1,w2) -> first f w1
  | VComb (w1,w2) -> first f w1
  | NoFrame _ -> ()
(*e: function Window.first *)

(*s: function Window.last *)
let rec last f window =
  match window.win_down with
    WFrame frame -> f frame
  | HComb (w1,w2) -> last f w2
  | VComb (w1,w2) -> last f w2
  | NoFrame _ -> ()
(*e: function Window.last *)

(*s: function Window.next *)
let rec next f window =
  match window.win_up with
  | TopWindow top_window -> first f top_window.window
  | Window win ->
      match win.win_down with
        HComb (w1,w2) ->
          if w2 == window 
          then next f win
          else first f w2
      | VComb (w1,w2) ->
          if w2 == window 
          then next f win
          else first f w2
      | _ -> ()
(*e: function Window.next *)
      
(*s: function Window.prev *)
let rec prev f window =
  match window.win_up with
    TopWindow top_window -> 
      if window == top_window.window then ()
      else 
        last f top_window.window
  | Window win ->
      match win.win_down with
        HComb (w1,w2) ->
          if w1 == window 
          then prev f win
          else last f w1
      | VComb (w1,w2) ->
          if w1 == window 
          then prev f win
          else last f w1
      | _ -> ()
(*e: function Window.prev *)

(*s: function Window.xterm *)
let backend top_window =
  match top_window.graphics with
    None -> raise Not_found
  | Some x -> x
(*e: function Window.xterm *)

(*s: function Window.get_font *)
let get_font font_name =
  let location = Efuns.location() in
  try
    Hashtbl.find location.loc_fonts font_name
  with Not_found ->
    if location.loc_fonts_n = 256 
    then raise Not_found
    else begin
      let n = location.loc_fonts_n in
      location.loc_fonts_n <- n + 1;
      location.loc_fonts_names.(n) <- font_name;
      Hashtbl.add location.loc_fonts font_name n;
      n
    end
(*e: function Window.get_font *)
        
(*s: function Window.get_color *)
let get_color color_name =
  let location = Efuns.location() in
  try
    Hashtbl.find location.loc_colors color_name
  with Not_found ->
    if location.loc_colors_n = 256 
    then raise Not_found
    else begin
      let n = location.loc_colors_n in
      location.loc_colors_n <- n + 1;
      location.loc_colors_names.(n) <- color_name;
      Hashtbl.add location.loc_colors color_name n;
      n
    end
(*e: function Window.get_color *)
        
(*e: core/window.ml *)
