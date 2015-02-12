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
module Color = Simple_color

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type world = {
  model: Efuns.location;

  (* viewport, device coordinates *)
  mutable width:  int;
  mutable height: int;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

module ArithFloatInfix = struct
    let (+..) = (+)
    let (-..) = (-)
    let (/..) = (/)
    let ( *.. ) = ( * )


    let (+) = (+.)
    let (-) = (-.)
    let (/) = (/.)
    let ( * ) = ( *. )

    let (+=) ref v = ref := !ref + v
    let (-=) ref v = ref := !ref - v

end
(* floats are the norm in cairo *)
open ArithFloatInfix


(*****************************************************************************)
(* Cairo helpers *)
(*****************************************************************************)

(* was in pfff/.../cairo_helpers.ml *)
let set_source_color ?(alpha=1.) ~cr ~color () = 
  (let (r,g,b) = color |> Color.rgbf_of_string in
  Cairo.set_source_rgba cr r g b alpha;
  )

let show_text cr s =
  if s = "" 
  then () 
  else 
    Cairo.show_text cr s

(*****************************************************************************)
(* Draw API *)
(*****************************************************************************)

(* helper *)
let move_to cr col line =
   let extent = Cairo.text_extents cr "peh" in
   let (w,h) = extent.Cairo.text_width / 3., extent.Cairo.text_height in
   Cairo.move_to cr (w * col) (line * h)

let clear_eol cr  col line len =
  pr2 "TODO_eol"
(*
  pr2 (spf "WX_xterm.clear_eol: %d %d %d" col line len);
  move_to col line;
  let (w,h) = Graphics.text_size "d" in
  Graphics.set_color Graphics.white;
  Graphics.fill_rect (Graphics.current_x()) (Graphics.current_y())
    (w * len) h;
  ()
*)

let draw_string cr   col line  str  offset len   attr =
  pr2 (spf "WX_xterm.draw_string %f %f %s %d %d %d"
         col line str offset len attr);

(*
  let extent = Cairo.text_extents cr "d" in
  let (w,h) = extent.Cairo.text_width, extent.Cairo.text_height in
  move_to cr col line;
  set_source_color ~cr ~color:"white" ();
  fill_rectangle_xywh ~x ~y ~w:(w * len) ~h;
*)
  set_source_color ~cr ~color:"black" ();

  move_to cr col line;
  show_text cr (String.sub str offset len);
  ()

let update_displays cr =
  pr2 ("WX_xterm.update_displays")


let backend cr = 
  let conv x = float_of_int x in
  { Xdraw. 
  clear_eol = (fun a b c -> 
    clear_eol cr (conv a) (conv b) c); 
  draw_string = (fun a b c d e f -> 
    draw_string cr (conv a) (conv b) c d e f);
  update_displays = (fun () -> 
    update_displays cr);
}



(*****************************************************************************)
(* Test UI *)
(*****************************************************************************)

let width = 500
let height = 500

let test_draw cr =
  (* [0,0][1,1] world scaled to a width x height screen *)
  Cairo.scale cr (float_of_int width) (float_of_int height);

  Cairo.set_source_rgba cr ~red:0.5 ~green:0.5 ~blue:0.5 ~alpha:0.5;
  Cairo.set_line_width cr 0.001;

  Cairo.move_to cr 0.5 0.5;
  Cairo.line_to cr 0.6 0.6;
  Cairo.stroke cr;

  Cairo.select_font_face cr "serif"
    Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_BOLD;
  Cairo.set_font_size cr 0.1;

  let _extent = Cairo.text_extents cr "peh" in
  (* weird: if Cairo.text_extents cr "d" create an Out_of_memory exn *)

  Cairo.move_to cr 0.1 0.1;
  Cairo.show_text cr "THIS IS SOME TEXT";
  Cairo.move_to cr 0.1 0.2;
  Cairo.show_text cr "THIS IS SOME TEXT";
  Cairo.set_font_size cr 0.05;
  Cairo.move_to cr 0.1 0.3;
  Cairo.show_text cr "THIS IS SOME TEXT";

  Cairo.set_source_rgb cr ~red:0.1 ~green:0.1 ~blue:0.1;
  Cairo.move_to cr 0.1 0.1;
  Cairo.line_to cr 0.1 0.2;
  Cairo.stroke cr;

  let start = ref 0.0 in

  for _i = 0 to 3 do
    let end_ = !start +. 0.5 in
    Cairo.arc cr ~xc:0.5 ~yc:0.5 ~radius:0.3 ~angle1:!start
      ~angle2:end_;
    Cairo.stroke cr;
    start := end_;
  done;

  ()

let test_cairo () =
  let _locale = GtkMain.Main.init () in
  let w = GWindow.window ~title:"test" () in
  (w#connect#destroy GMain.quit) +> ignore;
  let px = GDraw.pixmap ~width ~height ~window:w () in
  px#set_foreground `WHITE;
  px#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
  let cr = Cairo_lablgtk.create px#pixmap in
  test_draw cr;
  (GMisc.pixmap px ~packing:w#add ()) +> ignore;
  w#show ();
  GMain.main()


(*****************************************************************************)
(* Final view rendering *)
(*****************************************************************************)

let paint w =
  pr2 "paint";
  Top_window.update_display w.model 


(*****************************************************************************)
(* The main UI *)
(*****************************************************************************)

let init2 location displayname =
  let _locale = GtkMain.Main.init () in

  (* those are a first guess. The first configure ev will force a resize *)
  let width = 600 in
  let height = 600 in
  let w = {
    model = location;
    width;
    height;
  }
  in

  let win = GWindow.window ~title:"Efuns" () in
  let quit () = GMain.Main.quit (); in

  (*-------------------------------------------------------------------*)
  (* Creation of core DS of Efuns (buffers, frames, top_window) *)
  (*-------------------------------------------------------------------*)

  let display = "" in
  let top_window = Top_window.create location display in

  let _ = Interactive.create_bindings location in

  (* open the first buffers *)
  !init_files +> List.iter (fun name ->
    let _ = Frame.load_file top_window.window name in ()
  );

  (*-------------------------------------------------------------------*)
  (* Layout *)
  (*-------------------------------------------------------------------*)

  let px = GDraw.pixmap ~width ~height ~window:win () in
  px#set_foreground `WHITE;
  px#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
  let cr = Cairo_lablgtk.create px#pixmap in
(*  Cairo.scale cr (float_of_int width) (float_of_int height); *)
  Cairo.scale cr 1.0 1.0;
  Cairo.select_font_face cr "serif"
    Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_BOLD;
  Cairo.set_font_size cr 10.0;

  top_window.graphics <- Some (backend cr); 
  paint w;
  (GMisc.pixmap px ~packing:win#add ()) +> ignore;

  (*-------------------------------------------------------------------*)
  (* End *)
  (*-------------------------------------------------------------------*)

  win#connect#destroy ~callback:quit |> ignore;
  win#show ();
  GMain.main()

let init a b =
  if !Efuns.check
  then test_cairo ()
  else init2 a b
