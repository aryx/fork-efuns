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

let top window = 
  let rec iter window =
    match window.win_up with
      TopWindow top_window -> top_window
    | Window window -> iter window
  in
  iter window

let iter f window = 
  let rec iter1 window =
    match window.win_down with
    | HComb (w1,w2) -> iter1 w1; iter1 w2       
    | VComb (w1,w2) -> iter1 w1; iter1 w2
    | WFrame frame -> f frame
    | NoFrame _ -> ()
  in
    iter1 window

let rec first f window =
  match window.win_down with
    WFrame frame -> f frame
  | HComb (w1,w2) -> first f w1
  | VComb (w1,w2) -> first f w1
  | NoFrame _ -> ()

let rec last f window =
  match window.win_down with
    WFrame frame -> f frame
  | HComb (w1,w2) -> last f w2
  | VComb (w1,w2) -> last f w2
  | NoFrame _ -> ()

let rec next f window =
  match window.win_up with
    TopWindow top_window -> first f top_window.top_windows
  | Window win ->
      match win.win_down with
        HComb (w1,w2) ->
          if w2 == window then
            next f win
          else
            first f w2
      | VComb (w1,w2) ->
          if w2 == window then
            next f win
          else
            first f w2
      | _ -> ()
      
let rec prev f window =
  match window.win_up with
    TopWindow top_window -> 
      if window == top_window.top_windows then ()
      else 
        last f top_window.top_windows
  | Window win ->
      match win.win_down with
        HComb (w1,w2) ->
          if w1 == window then
            prev f win
          else
            last f w1
      | VComb (w1,w2) ->
          if w1 == window then
            prev f win
          else
            last f w1
      | _ -> ()

let xterm top_window =
  match top_window.top_xterm with
    None -> raise Not_found
  | Some xterm -> xterm 

let display top_window =
  match top_window.top_display with
    None -> raise Not_found
  | Some display -> display

let get_font location font_name =
  try
    Hashtbl.find location.loc_fonts font_name
  with
    Not_found ->
      if location.loc_fonts_n = 256 then
        raise Not_found
      else
        let n = location.loc_fonts_n in
        location.loc_fonts_n <- n + 1;
        location.loc_fonts_names.(n) <- font_name;
        Hashtbl.add location.loc_fonts font_name n;
        n
        
let get_color location color_name =
  try
    Hashtbl.find location.loc_colors color_name
  with
    Not_found ->
      if location.loc_colors_n = 256 then
        raise Not_found
      else
        let n = location.loc_colors_n in
        location.loc_colors_n <- n + 1;
        location.loc_colors_names.(n) <- color_name;
        Hashtbl.add location.loc_colors color_name n;
        n
        
