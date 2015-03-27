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
open Options
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

type metrics = {
  font_width: float;
  font_height: float;
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


(*let re_space = Str.regexp "^[ ]+$"*)

(* !does side effect on the (mutable) string! *)
let prepare_string s = 
(*  if s ==~ re_space then  s ^ s (* double it *) else  *)
  begin
    for i = 0 to String.length s -.. 1 do
      let c = String.get s i in
      if int_of_char c >= 128
      then String.set s i 'Z'
      else 
        if c = '\t'
        then String.set s i ' '
      else ()
    done;
    s
  end
(* TODO use charreprs instead? *)


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
let move_to cr pg col line =
  let (_, metrics) = pg in
  let w = metrics.font_width in
  let h = metrics.font_height in
  Cairo.move_to cr (w * col) ((line * h) + h * 0.1)

(* ugly hacks below but had many graphic glitches; cairo 
 * floats are imprecise?
 *)

let clear_eol ?(color="DarkSlateGray") cr pg  col line len =
(*  pr2 (spf "WX_xterm.clear_eol: %.f %.f %d" col line len); *)
  let (_, metrics) = pg in
  let w = metrics.font_width in
  let h = metrics.font_height in

  let x =  w * col +
    if color = "DarkSlateGray"
    then 0.
    else w * 0.1

  in
  let y = line * h +
    if color = "DarkSlateGray"
    then 0.
    else h * 0.2
  in
  let h = h *
    if color = "DarkSlateGray"
    then 1.04
    else 0.8
  in
  let w = w * (float_of_int len) + w * 
    (if color = "DarkSlateGray"
     then 0.04 
     else -. 0.1
    )
  in
  (* to debug use draw and pink color ! so get bounding clear box *)
(*  draw_rectangle_xywh ~cr ~x ~y ~w:(w * (float_of_int len)) ~h ~color:"pink" ();*)
  fill_rectangle_xywh ~cr ~x ~y ~w ~h ~color (); 
  ()

let draw_string loc cr pg   col line  str  offset len   attr =
  if !debug_graphics
  then pr2 (spf "WX_xterm.draw_string %.f %.f \"%s\" %d %d attr = %d" 
              col line str offset len attr);
  let bgcolor = 
    let idx = (attr lsr 8) land 255 in
    loc.loc_colors_names.(idx)
  in
  clear_eol ~color:bgcolor cr pg col line len;
  move_to cr pg col line;
  let fgcolor = 
    let idx = (attr) land 255 in
    loc.loc_colors_names.(idx)
  in
  set_source_color ~cr ~color:fgcolor ();
  let (ly, _) = pg in
  Pango.Layout.set_text ly  (prepare_string (String.sub str offset len));
  Pango_cairo.update_layout cr ly;
  Pango_cairo.show_layout cr ly;
  ()


let backend loc cr pg win = 
  let conv x = float_of_int x in
  { Xdraw. 
    clear_eol = (fun a b c -> 
      clear_eol cr pg (conv a) (conv b) c); 
    draw_string = (fun a b c d e f -> 
      draw_string loc cr pg (conv a) (conv b) c d e f);
    update_display = (fun () -> 
      if !debug_graphics
      then pr2 ("backend.update_display()");
      GtkBase.Widget.queue_draw win#as_widget;
    );
  }


(*****************************************************************************)
(* Final view rendering *)
(*****************************************************************************)

let paint w =
  if !debug_graphics
  then pr2 "paint";
  Top_window.update_display () 

(* for the special key, Control, Meta, etc *)
let modifiers = ref 0

(*****************************************************************************)
(* The main UI *)
(*****************************************************************************)

let init2 init_files =
  let _locale = GtkMain.Main.init () in
  let location = Efuns.location () in

  let width = 1320 in
  let height = 1400 in
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

  (* location.loc_height <- 45; *)
  (* will boostrap and use a newly created *help* buffer *)
  let top_window = Top_window.create () in
  (* the *bindings* buffer *)
  Interactive.create_bindings () |> ignore;
  (* open the first buffers *)
  init_files |> List.iter (fun name ->
    Frame.load_file top_window.window name |> ignore
  );

  (*-------------------------------------------------------------------*)
  (* Layout *)
  (*-------------------------------------------------------------------*)


  let vbox = GPack.vbox ~packing:win#add () in

    (* not necessary, can even be distracting, but can be useful
     * for beginners to have at least an help menu.
     *)
    let menubar = GMenu.menu_bar ~packing:vbox#add () in
      let factory = new GMenu.factory menubar in

      factory#add_submenu "_File" |> (fun menu -> 
        GToolbox.build_menu menu ~entries:
          (!!Top_window.file_menu |> List.map (fun (str, action) ->
            if str = "" 
            then `S
            else `I (str, (fun () -> 
              let frame = top_window.top_active_frame in
              execute_action action frame;
              paint w
            ))))
      ) |> ignore;
      factory#add_submenu "_Edit" |> (fun menu -> 
        GToolbox.build_menu menu ~entries:
          (!!Top_window.edit_menu |> List.map (fun (str, action_name) ->
            match str with
            | "" -> `S
            | _ -> `I (str, (fun () -> 
              let frame = top_window.top_active_frame in
              execute_action action_name frame;
              paint w;
            ))
           ))
      ) |> ignore;

      factory#add_submenu "_Buffers" |> (fun menu -> 
        (* TODO *)
        ()
      );
      factory#add_submenu "_Help" |> (fun menu -> 
        GToolbox.build_menu menu ~entries:
          (!Top_window.help_menu 
           |> Array.to_list 
           |> List.map (fun (str, action) ->
                `I (str, (fun () -> 
                  let frame = top_window.top_active_frame in
                  action frame;
                  paint w;
                ))))
      ) |> ignore;

    let px = GDraw.pixmap ~width ~height ~window:win () in
    px#set_foreground `BLACK;
    px#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
    GMisc.pixmap px ~packing:vbox#add () |> ignore;

    let _statusbar = GMisc.statusbar ~packing:vbox#add () in

  (*-------------------------------------------------------------------*)
  (* Cairo/pango setup *)
  (*-------------------------------------------------------------------*)

  let cr = Cairo_lablgtk.create px#pixmap in
  let layout = Pango_cairo.create_layout cr in
  let desc = Pango.Font.from_string location.loc_font
(*
    "Sans Bold 25" 
    "Fixed Bold 32"
    "Monaco 16"
    "Menlo 19"
    "Courier 19"
    "Menlo 18"
*)
  in
  Pango.Font.set_weight desc `ULTRABOLD; 
  Pango.Layout.set_font_description layout desc;


  let ctx = Pango_cairo.FontMap.create_context 
    (Pango_cairo.FontMap.get_default ()) in
  Pango.Context.set_font_description ctx desc;
  let metrics = 
    Pango.Context.get_metrics ctx 
      (Pango.Context.get_font_description ctx) None in
  let width = 
    float_of_int (Pango.Font.get_approximate_char_width metrics) / 1024. in
  let descent = float_of_int (Pango.Font.get_descent metrics) / 1024. in
  let ascent =  float_of_int (Pango.Font.get_ascent metrics) / 1024. in
  let height = ascent + descent in
  let metrics = { 
    font_width = width; 
    font_height = height * 1.1;
  } in
  let pg = (layout, metrics) in

  Pango_cairo.update_layout cr layout;

  top_window.graphics <- Some (backend location cr pg win); 

  for i = 0 to (Efuns.location()).loc_height -.. 1 do
    clear_eol cr pg 0. (float_of_int i) 80;
  done;
  paint w;

  (*-------------------------------------------------------------------*)
  (* Events *)
  (*-------------------------------------------------------------------*)

  win#event#connect#key_press ~callback:(fun key ->
    if !debug
    then pr2 (spf "key: %d, %s" 
                (GdkEvent.Key.keyval key) (GdkEvent.Key.string key));

    let code_opt =
      match GdkEvent.Key.keyval key with

      | 65289 -> Some XK.xk_Tab
      | 65288 -> Some XK.xk_BackSpace

      | 65293 -> Some XK.xk_Return

      | 65361 -> Some XK.xk_Left
      | 65362 -> Some XK.xk_Up
      | 65363 -> Some XK.xk_Right
      | 65364 -> Some XK.xk_Down
      | 65365 -> Some XK.xk_Prior
      | 65366 -> Some XK.xk_Next

      | 65507 -> modifiers := !modifiers lor Xtypes.controlMask; None

      | 65511 -> modifiers := !modifiers lor Xtypes.mod1Mask; None

      | x when x > 65000 -> None

      | x -> Some x
    in
    code_opt |> Common.do_option (fun code ->
      let evt = Xtypes.XTKeyPress (!modifiers, GdkEvent.Key.string key, code) in
      Top_window.handler top_window evt;
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

  (*-------------------------------------------------------------------*)
  (* End *)
  (*-------------------------------------------------------------------*)

  GtkSignal.user_handler := (fun exn -> 
    pr2 "fucking callback";
    let s = Printexc.get_backtrace () in
    pr2 s;
(*
    let pb = "pb: " ^ Common.exn_to_s exn in
    G.dialog_text ~text:pb ~title:"pb";
*)
    raise exn
  );

  win#connect#destroy ~callback:quit |> ignore;
  win#show ();
  GMain.main()










(*****************************************************************************)
(* Test cairo/gtk *)
(*****************************************************************************)

let width = 500
let height = 500

let test_draw cr =
  (* [0,0][1,1] world scaled to a width x height screen *)
(*  Cairo.scale cr (float_of_int width) (float_of_int height); *)

  Cairo.set_source_rgba cr ~red:0.5 ~green:0.5 ~blue:0.5 ~alpha:0.5;
  Cairo.set_line_width cr 0.001;

  Cairo.move_to cr 0.5 0.5;
  Cairo.line_to cr 0.6 0.6;
  Cairo.stroke cr;

(*
  Cairo.select_font_face cr "monospace"
    Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_BOLD;
*)
  Cairo.select_font_face cr "fixed"
    Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_NORMAL;

  Cairo.set_font_size cr 0.05;

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

  let layout = Pango_cairo.create_layout cr in
  let ctx = Pango_cairo.FontMap.create_context 
    (Pango_cairo.FontMap.get_default ()) in


  Pango.Layout.set_text layout "let x = 1 in main () for x = 1 to 3!";
  let desc = Pango.Font.from_string 
(*
    "Sans Bold 25" 
    "Fixed Bold 32"
*)
    "b&h-Luxi Bold 23"
  in
  Pango.Context.set_font_description ctx desc;
  Pango.Layout.set_font_description layout desc;
  Pango_cairo.update_layout cr layout;

  Cairo.move_to cr 0. 0.;
  Pango_cairo.show_layout cr layout;
  pr2 (spf "font = %s" (Pango.Font.to_string desc));


  let metrics = 
    Pango.Context.get_metrics ctx 
      (Pango.Context.get_font_description ctx) None in
  let w = 
    float_of_int (Pango.Font.get_approximate_char_width metrics) / 1024. in
  let descent = 
    float_of_int (Pango.Font.get_descent metrics) / 1024. in
  let ascent = 
    float_of_int (Pango.Font.get_ascent metrics) / 1024. in
  let h = ascent + descent in
  pr2_gen (w, h, ascent, descent);

  Cairo.move_to cr 0. h;
  Pango_cairo.show_layout cr layout;

  ()



let test_cairo () =
  let _locale = GtkMain.Main.init () in
  let w = GWindow.window ~title:"test" () in
  (w#connect#destroy GMain.quit) |> ignore;
  let px = GDraw.pixmap ~width ~height ~window:w () in
  px#set_foreground `WHITE;
  px#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
  let cr = Cairo_lablgtk.create px#pixmap in
  test_draw cr;
  (GMisc.pixmap px ~packing:w#add ()) |> ignore;
  w#show ();
  GMain.main()

(*****************************************************************************)
(*****************************************************************************)

let init a =
  if !Efuns.check
  then test_cairo ()
  else init2 a
