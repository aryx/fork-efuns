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
module Color = Simple_color

open Efuns

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
(* Cairo helpers *)
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
end
(* floats are the norm in cairo *)
open ArithFloatInfix

(* was in pfff/.../cairo_helpers.ml *)
let set_source_color ?(alpha=1.) ~cr ~color () = 
  (let (r,g,b) = color |> Color.rgbf_of_string in
  Cairo.set_source_rgba cr r g b alpha;
  )

(* Text cairo API seems buggy, especially on MacOS X, see
 * https://github.com/diagrams/diagrams-cairo/issues/43
 * so I had to hack many things in move_to() by using different extents..
 * Indeed the main cairo documents says Cairo.text_xxx are a "toy text API"
 * http://cairographics.org/manual/cairo-text.html
 *)
(* less: prepare_string thing *)
let show_text cr s =
  if s = "" 
  then () 
  else 
    Cairo.show_text cr s



let fill_rectangle_xywh ?alpha ~cr ~x ~y ~w ~h ~color () = 
  set_source_color ?alpha ~cr ~color ();
  
  Cairo.move_to cr x y;
  Cairo.line_to cr (x+w) y;
  Cairo.line_to cr (x+w) (y+h);
  Cairo.line_to cr x (y+h);
  Cairo.fill cr;
  ()

let draw_rectangle_xywh ?alpha ~cr ~x ~y ~w ~h ~color () = 
  set_source_color ?alpha ~cr ~color ();
  
  Cairo.move_to cr x y;
  Cairo.line_to cr (x+w) y;
  Cairo.line_to cr (x+w) (y+h);
  Cairo.line_to cr x (y+h);
  Cairo.stroke cr;
  ()

(*****************************************************************************)
(* Draw Efuns API *)
(*****************************************************************************)

(* helper *)
let move_to cr col line =
   let extent = Cairo.font_extents cr in
   (* I thought extent.max_x_advance would be good for the width, but it's not,
    * extent2.text_width seems better.
    *)
   let extent2 = Cairo.text_extents cr "peh" in
   let (w,h) = extent2.Cairo.text_width / 3., extent.Cairo.font_height in
   Cairo.move_to cr (w * col) (line * h + h - extent.Cairo.descent)

let clear_eol ?(color="DarkSlateGray") cr  col line len =
(*  pr2 (spf "WX_xterm.clear_eol: %.f %.f %d" col line len); *)
  let extent = Cairo.font_extents cr in
  let extent2 = Cairo.text_extents cr "peh" in
  let (w,h) = extent2.Cairo.text_width / 3., extent.Cairo.font_height in
  let x, y = w * col, line * h in
  (* to debug use draw and pink color ! so get bounding clear box *)
  (* draw_rectangle_xywh ~cr ~x ~y ~w:(w * (float_of_int len)) ~h ~color:"pink" (); *)
  fill_rectangle_xywh ~cr ~x ~y ~w:(w * (float_of_int len)) ~h ~color ();
  ()

let draw_string cr   col line  str  offset len   attr =
  pr2 (spf "WX_xterm.draw_string %.f %.f \"%s\" %d %d attr = %d" 
    col line str offset len attr);
  let color = if attr = Text.inverse_attr then "wheat" else "DarkSlateGray" in
  clear_eol ~color cr col line len;
  move_to cr col line;
  let color = if attr = Text.inverse_attr then "DarkSlateGray" else "wheat" in
  set_source_color ~cr ~color ();
  show_text cr (String.sub str offset len);
  ()

let update_displays cr =
  pr2 ("WX_xterm.update_displays")


let backend cr = 
  let conv x = float_of_int x in
  { Xdraw. 
    clear_eol = (fun a b c -> clear_eol cr (conv a) (conv b) c); 
    draw_string = (fun a b c d e f -> draw_string cr (conv a) (conv b) c d e f);
    update_displays = (fun () -> update_displays cr);
  }


(*****************************************************************************)
(* Test cairo/gtk *)
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

  Cairo.select_font_face cr "monospace"
    Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_BOLD;
  Cairo.set_font_size cr 0.1;

  let _extent = Cairo.text_extents cr "peh" in
  (* WEIRD: if Cairo.text_extents cr "d" create an Out_of_memory exn *)
  (* related? https://github.com/diagrams/diagrams-cairo/issues/43
  *)


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

(* for the special key, Control, Meta, etc *)
let modifiers = ref 0

(*****************************************************************************)
(* The main UI *)
(*****************************************************************************)

let init2 location =
  let _locale = GtkMain.Main.init () in

  let width = 800 in
  let height = 1010 in
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

  location.loc_height <- 50;
  (* will boostrap and use a newly created *help* buffer *)
  let top_window = Top_window.create location in
  (* the *bindings* buffer *)
  Interactive.create_bindings location |> ignore;
  (* open the first buffers *)
  !init_files +> List.iter (fun name ->
    Frame.load_file top_window.window name |> ignore
  );

  (*-------------------------------------------------------------------*)
  (* Layout *)
  (*-------------------------------------------------------------------*)

  let px = GDraw.pixmap ~width ~height ~window:win () in
  px#set_foreground `WHITE;
  px#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();

  let cr = Cairo_lablgtk.create px#pixmap in
  Cairo.scale cr 1.0 1.0;
  Cairo.select_font_face cr "fixed"
    Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_NORMAL;
  Cairo.set_font_size cr 20.0;


  top_window.graphics <- Some (backend cr); 
  paint w;

  win#event#connect#key_press ~callback:(fun key ->
    pr2 (spf "%d, %s" (GdkEvent.Key.keyval key) (GdkEvent.Key.string key));

    let code_opt =
      match GdkEvent.Key.keyval key with
      | 65293 -> Some XK.xk_Return
      | 65288 -> Some XK.xk_BackSpace
      | 65361 -> Some XK.xk_Left
      | 65362 -> Some XK.xk_Up
      | 65363 -> Some XK.xk_Right
      | 65364 -> Some XK.xk_Down

      | 65289 -> Some XK.xk_Tab

      | 65507 -> modifiers := !modifiers lor Xtypes.controlMask; None
      | 65511 -> modifiers := !modifiers lor Xtypes.mod1Mask; None

      | x -> Some x
    in
    code_opt |> Common.do_option (fun code ->
      let evt = Xtypes.XTKeyPress (!modifiers, GdkEvent.Key.string key, code) in
      Top_window.handler top_window evt;
      GtkBase.Widget.queue_draw win#as_widget;
    );
    true
  ) |> ignore;
  win#event#connect#key_release ~callback:(fun key ->
    (match GdkEvent.Key.keyval key with
    | 65507 -> modifiers := !modifiers land (lnot Xtypes.controlMask)
    | 65511 -> modifiers := !modifiers land (lnot Xtypes.mod1Mask)
    | _ -> ()
    );
    true
  ) |> ignore;

  (GMisc.pixmap px ~packing:win#add ()) |> ignore;

  (*-------------------------------------------------------------------*)
  (* End *)
  (*-------------------------------------------------------------------*)

  win#connect#destroy ~callback:quit |> ignore;
  win#show ();
  GMain.main()

let init a =
  if !Efuns.check
  then test_cairo ()
  else init2 a
