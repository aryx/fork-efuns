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
  (* the "model" (but actually accessible also via Globals.location()) *)
  loc: Efuns.editor;

  (* dimensions *)
  metrics: layout;

  (* first cairo layer, for heavy computation e.g. the minimap *)
  mutable base: [ `Any ] Cairo.surface;
  (* second cairo layer, for scrolling window on minimap *)
  mutable overlay: [ `Any ] Cairo.surface;
  (* the final drawing area *)
  mutable final: [ `Any ] Cairo.surface;

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
(* the code below is necessary when using the (limited/buggy) toy cairo text
 * api. It seems also necessary when using pango as I get some
 * Pango warning about UTF-8 characters and ugly display.
 *)
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

(* see http://cairographics.org/FAQ/#clear_a_surface *)
let clear cr =
  Cairo.set_source_rgba cr 0. 0. 0.   0.;
  Cairo.set_operator cr Cairo.OPERATOR_SOURCE;
  Cairo.paint cr;
  Cairo.set_operator cr Cairo.OPERATOR_OVER;
  ()

(*****************************************************************************)
(* Pango and metrics *)
(*****************************************************************************)

let compute_metrics loc desc =

  let fontmap = Pango_cairo.FontMap.get_default () in
  let ctx = Pango_cairo.FontMap.create_context fontmap in
  Pango.Context.set_font_description ctx desc;

  let metrics = Pango.Context.get_metrics ctx desc None in
  let width = 
    float_of_int (Pango.Font.get_approximate_char_width metrics) / 1024. in
  let descent = float_of_int (Pango.Font.get_descent metrics) / 1024. in
  let ascent =  float_of_int (Pango.Font.get_ascent metrics) / 1024. in
  (* CONFIG? *)
  let height = (ascent + descent) * 1.1 in

  let metrics = { 
    font_width = width; 
    font_height = height;

    main_width = float_of_int loc.loc_width * width;
    main_height = float_of_int loc.loc_height * height;

    mini_factor = 10.;
    margin_factor = 150.;

    (* set later; derived from above (see the code below) *)
    linemax = 0;
    mini_width = 0.;
    margin_width = 0.;
    full_width = 0;
    full_height = 0;
  }
  in
  metrics.linemax <- 
   metrics.main_height * metrics.mini_factor / metrics.font_height 
       |> ceil |> int_of_float;
  metrics.mini_width <- metrics.main_width / metrics.mini_factor;
  metrics.margin_width <- metrics.main_width / metrics.margin_factor;

  (* those are the dimensions for the main view, the drawing area *)
  metrics.full_width <- 
    metrics.main_width + metrics.mini_width + metrics.margin_width * 2.
       |> ceil |>int_of_float;
    (* 1320 *)
  metrics.full_height <- metrics.main_height |> ceil |> int_of_float;
    (* 1400 *)
  
  metrics

let pango_layout cr desc =
  let layout = Pango_cairo.create_layout cr in
  Pango.Layout.set_font_description layout desc;
  Pango_cairo.update_layout cr layout;
  layout
let pango_layout a b =
  Common.profile_code "G.pango_layout" (fun () -> pango_layout a b)


let pango_show_text ly cr str =
  Pango.Layout.set_text ly  (prepare_string str);
  Pango_cairo.update_layout cr ly;
  Pango_cairo.show_layout cr ly

let pango_show_text a b c =
  Common.profile_code "G.pango_show_text" (fun () -> pango_show_text a b c)

(*****************************************************************************)
(* Minimap *)
(*****************************************************************************)

(* minimap, a la Code Thumbnails, Sublime text, or many regular app
 * like powerpoint, Preview, etc where have thumbnails preview.
 * Focus+context!
 *)
let draw_minimap w =

  let cr = Cairo.create w.base in
  Cairo.translate cr (w.metrics.main_width + w.metrics.margin_width *. 2.) 0.0;

  fill_rectangle_xywh ~cr ~x:0. ~y:0. 
    ~w:w.metrics.mini_width ~h:w.metrics.main_height
    ~color:"grey22" ();

  Cairo.scale cr (1. / w.metrics.mini_factor) (1. / w.metrics.mini_factor);

  let frame = w.loc.top_windows |> List.hd |> (fun tw -> tw.top_active_frame) in
  let buf = frame.frm_buffer in
  let text = buf.buf_text in

  let line = Text.point_line text frame.frm_start in
  (* ex: linemax = 400, line = 10 => startpage = 0; line = 410 => page = 1 *)
  let startpage = line /.. w.metrics.linemax in
  let startline = startpage *.. w.metrics.linemax in
  let endline = 
    min (startline +.. w.metrics.linemax) (Text.nbr_lines text -.. 1) in


  for i = startline to endline do
    let line = Text.compute_representation text buf.buf_charreprs i in
    let repr_str = line.Text.repr_string in
    line.Text.boxes |> List.rev |> List.iter (fun box ->
      (* opti: no need to spend time on long lines, their tail is not
       * displayed. Useful in eshell buffers with long compilation lines.
       *)
      if box.Text.box_col >= 100
      then () 
      else  begin
      let h = w.metrics.font_height in
      let x = float_of_int box.Text.box_pos_repr * w.metrics.font_width in
      let line_in_page = i mod w.metrics.linemax in
      let y = (float_of_int line_in_page * h) + h * 0.1 in
      Cairo.move_to cr x y;
      let attr = box.Text.box_attr in

      let str = String.sub repr_str box.Text.box_pos_repr box.Text.box_size in

      let fgcolor = 
        let idx = attr land 255 in
        w.loc.loc_colors_names.(idx)
      in
      let fontsize = (attr lsr 16) land 255 in
      set_source_color ~cr ~color:fgcolor ();

      let ly = 
        if fontsize = 0 
        then w.ly
        else begin
          (* def of func is fontsize 3 *)
          let size = 30 +.. 20 *.. fontsize in
          (* Does not have to be "fixed ..." here, we want to optimize for
           * readability. Actually we can move to the bol
           * so preceding long tokens like \subsubsection do not push
           * us too much to the right.
           *)
          Cairo.move_to cr 0. y;
          let desc = Pango.Font.from_string (spf "Serif %d" size) in
          pango_layout cr desc
        end
      in
      pango_show_text ly cr str

(*
  this generate some out_of_memory error when run directly efuns
  on lexer_nw.mll. weird, but Cairo text api is known to be buggy.
*)
(*
      Cairo.select_font_face cr "serif"
        Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_NORMAL;
      Cairo.set_font_size cr (38. + 25. * (float_of_int fontsize));
      Cairo.show_text cr (prepare_string str);
*)
      end
    )
  done;
  ()

let draw_minimap a = Common.profile_code "G.draw_minimap" 
  (fun () -> draw_minimap a)

let draw_minimap_overlay w =

  let cr = Cairo.create w.overlay in
  clear cr;

  Cairo.translate cr (w.metrics.main_width + w.metrics.margin_width * 2.) 0.0;
  Cairo.scale cr (1. / w.metrics.mini_factor) (1. / w.metrics.mini_factor);

  let frame = w.loc.top_windows |> List.hd |> (fun tw -> tw.top_active_frame) in
  let buf = frame.frm_buffer in
  let text = buf.buf_text in

  let line = Text.point_line text frame.frm_start in

  (* ex: linemax = 400, line = 10 => startpage = 0; line = 410 => page = 1 *)
  let startpage = line /.. w.metrics.linemax in
  let startline = startpage *.. w.metrics.linemax in

  let line = line -.. startline in

  let x = 0. in
  let y = (float_of_int line) * w.metrics.font_height in
  let h = (float_of_int w.loc.loc_height) * w.metrics.font_height in

  Cairo.set_line_width cr ((Cairo.get_line_width cr) * w.metrics.mini_factor);
  fill_rectangle_xywh ~alpha:0.2 ~cr ~x ~y ~w:w.metrics.main_width ~h
    ~color:"white" ();
  ()

let draw_minimap_overlay a = Common.profile_code "G.draw_minimap_overlay" 
  (fun () -> draw_minimap_overlay a)




(* opti to avoid recompute/redraw expensive minimap *)
let active_frame_info w =
  let frame = w.loc.top_windows |> List.hd |> (fun tw -> tw.top_active_frame) in
  let buf = frame.frm_buffer in
  let text = buf.buf_text in

  let line = Text.point_line text frame.frm_start in
  (* ex: linemax = 400, line = 10 => startpage = 0; line = 410 => page = 1 *)
  let startpage = line /.. w.metrics.linemax in

  buf.buf_name, Text.version text, startpage

let idle_minimap = ref None

let draw_minimap_when_idle w win =
  !idle_minimap |> Common.do_option (fun x ->
    GMain.Timeout.remove x;
  );
  idle_minimap := 
    Some (GMain.Timeout.add ~ms:50 ~callback:(fun () ->

      (* opti: avoid redrawing if nothing was modified and we just moved
       * the cursor 
       *)
      let active_frame = active_frame_info w in
      if active_frame <> w.last_top_frame_info
      then begin 
        (* todo: do in a thread when idle *)
        draw_minimap w;
        w.last_top_frame_info <- active_frame;
      end;
      draw_minimap_overlay w;

      (* this will trigger the expose event *)
      GtkBase.Widget.queue_draw win#as_widget;
      (* avoid double Idle.remove *)
      idle_minimap := None;
      false
    ))

(*****************************************************************************)
(* Draw Efuns API *)
(*****************************************************************************)

(* less: could rewrite to just take w instead of cr x pg *)

(* helper *)
let move_to cr pg col line =
  let (_, metrics) = pg in
  let w = metrics.font_width in
  let h = metrics.font_height in
  Cairo.move_to cr (w * col) ((line * h) + h * 0.1)

(* ugly hacks below but had many graphic glitches; cairo 
 * floats are imprecise?
 * CONFIG need to position to ascent point! otherwise some of the
 *  descent part can get erased (look at the 'y','g' chars, often the very
 *  top is erased)
 *)

let clear_eol ?(color="DarkSlateGray") cr pg  col line len =
  (*pr2 (spf "WX_xterm.clear_eol: %.f %.f %d, color = %s" col line len color);*)
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
  if !Globals.debug_graphics
  then pr2 (spf "WX_xterm.draw_string %.f %.f \"%s\" %d %d attr = %d" 
              col line str offset len attr);
  let bgcolor = 
    let idx = (attr lsr 8) land 255 in
    let idx = 
      if attr land Text.highlight_bit > 0
      then 2
      else idx
    in
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

let backend w da top_gtk_win = 
  let conv x = float_of_int x in

  let cr = Cairo.create w.base in
  Cairo.translate cr (w.metrics.margin_width) 0.0;
  let pg = (w.ly, w.metrics) in

  let loc = w.loc in
  { Xdraw. 
    clear_eol = (fun a b c -> 
      clear_eol cr pg (conv a) (conv b) c
    ); 
    draw_string = (fun a b c d e f -> 
      draw_string loc cr pg (conv a) (conv b) c d e f
    );

    (* refresh drawing area *)
    update_display = (fun () -> 
      if !Globals.debug_graphics
      then pr2 ("backend.update_display()");

      draw_minimap_when_idle w da;
      (* this will trigger the expose event *)
      GtkBase.Widget.queue_draw da#as_widget;
    );
    update_window_title = (fun s ->
      top_gtk_win#set_title s
    );
  }


(*****************************************************************************)
(* paint/configure/expose *)
(*****************************************************************************)

let paint () =
  (* this will trigger backend.update_display *)
  Top_window.update_display () 


let get_w = ref (fun () -> failwith "no w yet, configure has been called?")

let configure loc top_window desc metrics da top_gtk_win =
 fun ev ->
  let width = GdkEvent.Configure.width ev in
  let height = GdkEvent.Configure.height ev in

  (* todo: metrics should be recomputed by
   * first adjusting loc_width and loc_height
   *)

  (*-------------------------------------------------------------------*)
  (* Cairo/pango graphics backend setup *)
  (*-------------------------------------------------------------------*)
  let cr = Cairo_lablgtk.create (*px#pixmap*) da#misc#window in
  let surface = Cairo.get_target cr in

  let colorkind = Cairo.CONTENT_COLOR_ALPHA in

  let w = {
    loc = loc;
    base =    Cairo.surface_create_similar surface colorkind  width height;
    overlay = Cairo.surface_create_similar surface colorkind  width height;
    final = surface;
    ly = pango_layout cr desc;
    metrics;
    last_top_frame_info = ("", -1, -1);
  }
  in
  top_window.graphics <- Some (backend w da top_gtk_win); 
  get_w := (fun () -> w);

  let cr = Cairo.create w.base in

  fill_rectangle_xywh ~cr ~x:0. ~y:0. 
    ~w:(w.metrics.margin_width) 
    ~h:(float_of_int height)
    ~color:"Black" ();
  Cairo.translate cr (w.metrics.margin_width) 0.0;
  fill_rectangle_xywh ~cr ~x:0. ~y:0. 
    ~w:(float_of_int width) ~h:(float_of_int height)
    ~color:"DarkSlateGray" ();
  Cairo.translate cr (w.metrics.main_width) 0.0;
  fill_rectangle_xywh ~cr ~x:0. ~y:0. 
    ~w:(w.metrics.margin_width) 
    ~h:(float_of_int height)
    ~color:"Black" ();


  (* force a redraw for all the frame after a resize. w.base has changed! *)
  (Globals.location()).top_windows |> List.iter (fun top_window ->
     top_window.window |> Window.iter(fun frm -> frm.frm_redraw <- true;);
     (match top_window.top_mini_buffers with
      | [] -> ()
      | frm :: _ -> frm.frm_redraw <- true;
     );
  ); 
(*
  let pg = (layout, metrics) in
  for i = 0 to (Globals.location()).loc_height -.. 1 do
    clear_eol cr pg 0. (float_of_int i) 80;
  done;
*)

  paint ();
  true



let assemble_layers w =
  let surface_src = w.base in
  let cr_final = Cairo.create w.final in
  Cairo.set_operator cr_final Cairo.OPERATOR_OVER;
  Cairo.set_source_surface cr_final surface_src 0. 0.;
  Cairo.paint cr_final;
  Cairo.set_operator cr_final Cairo.OPERATOR_OVER;
  Cairo.set_source_surface cr_final w.overlay 0. 0.;
  Cairo.paint cr_final;
  ()

(* expose should do very little, it should be fast, because
 * this may be called every second after an update to some
 * graphics, e.g. the cursor thread
 *)
let expose _ev =
  let w = !get_w() in
  assemble_layers w;
  true

(*****************************************************************************)
(* The cursor *)
(*****************************************************************************)
let cnt = ref 0
let start_cursor_thread () =
  Thread.create (fun () ->
    while true do
      incr cnt;
      (*pr2 (spf "%d" !cnt);*)
      Thread.delay 0.5;
      Globals.with_lock (fun () ->
        (Globals.location()).top_windows |> List.iter (fun top_window ->
          if !cnt mod 2 = 0
          then Top_window.cursor_on top_window
          else Top_window.cursor_off top_window;
          let graphic = Efuns.backend top_window in
          graphic.Xdraw.update_display();
        )
      )
    done
  ) ()

(*****************************************************************************)
(* The main UI *)
(*****************************************************************************)

(* for the special key, Control, Meta, etc *)
let modifiers = ref 0

let init2 init_files =

  (*-------------------------------------------------------------------*)
  (* Graphics initialisation *)
  (*-------------------------------------------------------------------*)
  let _locale = GtkMain.Main.init () in
  let loc = Globals.location () in

  let desc = Pango.Font.from_string loc.loc_font
  (* see https://github.com/hbin/top-programming-fonts to install
   * some nice programming fonts (e.g., Menlo)
   *)
(*
    "Monospace 19"
    "Monaco 16"
    "Sans Bold 25" 
    "Fixed Bold 32"
    "Courier 19"
    "Menlo 18" <- current
*)
  in
  (* Pango.Font.set_weight desc `ULTRABOLD; *)
  pr2 (Pango.Font.to_string desc);

  let metrics = compute_metrics loc desc in

  (*-------------------------------------------------------------------*)
  (* Window creation *)
  (*-------------------------------------------------------------------*)

  let win = GWindow.window ~title:"Efuns" () in

  (*-------------------------------------------------------------------*)
  (* Creation of core DS of Efuns (buffers, frames, top_window) *)
  (*-------------------------------------------------------------------*)

  (* loc.loc_height <- 45; *)
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

    (*-------------------------------------------------------------------*)
    (* Menu *)
    (*-------------------------------------------------------------------*)
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
              Action.execute_action action frame;
              paint ()
            ))))
      ) |> ignore;
      factory#add_submenu "_Edit" |> (fun menu -> 
        GToolbox.build_menu menu ~entries:
          (!!Top_window.edit_menu |> List.map (fun (str, action_name) ->
            match str with
            | "" -> `S
            | _ -> `I (str, (fun () -> 
              let frame = top_window.top_active_frame in
              Action.execute_action action_name frame;
              paint ();
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
                  paint ();
                ))))
      ) |> ignore;

    (*-------------------------------------------------------------------*)
    (* main view *)
    (*-------------------------------------------------------------------*)

    let da = GMisc.drawing_area ~packing:vbox#add () in
    (* we manage ourselves layers with cairo *)
    da#misc#set_double_buffered false;
    da#set_size ~width:metrics.full_width ~height:metrics.full_height;
    da#misc#set_can_focus true ;
    da#event#add [ `BUTTON_MOTION; `POINTER_MOTION;
                   `BUTTON_PRESS; `BUTTON_RELEASE ];
    (* `KEY_PRESS; ? or let the even go to the window ? *)
(*
    let px = GDraw.pixmap ~width ~height ~window:win () in
    px#set_foreground `BLACK;
    px#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
    GMisc.pixmap px ~packing:vbox#add () |> ignore;
*)

    da#event#connect#configure 
      ~callback:(configure loc top_window desc metrics da win) |> ignore;
    da#event#connect#expose ~callback:(expose) |> ignore;

    (*-------------------------------------------------------------------*)
    (* status bar *)
    (*-------------------------------------------------------------------*)
    let _statusbar = GMisc.statusbar ~packing:vbox#add () in

  (*-------------------------------------------------------------------*)
  (* Events *)
  (*-------------------------------------------------------------------*)

  win#event#connect#key_press ~callback:(fun key ->
    if !Globals.debug
    then pr2 (spf "key: %d, %s" 
                (GdkEvent.Key.keyval key) (GdkEvent.Key.string key));

    let code_opt =
      match GdkEvent.Key.keyval key with

      (* special keys *)

      | 65289 -> Some XK.xk_Tab
      | 65288 -> Some XK.xk_BackSpace

      | 65293 -> Some XK.xk_Return

      | 65361 -> Some XK.xk_Left
      | 65362 -> Some XK.xk_Up
      | 65363 -> Some XK.xk_Right
      | 65364 -> Some XK.xk_Down
      | 65365 -> Some XK.xk_Prior
      | 65366 -> Some XK.xk_Next
        
      (* modifiers
       * coupling: modify also the release event
      *)

      | 65507 -> modifiers := !modifiers lor Xtypes.controlMask; None
      (* on macOS? *)
      | 65511 -> modifiers := !modifiers lor Xtypes.mod1Mask; None
      (* on Linux: Alt *)
      | 65513 -> modifiers := !modifiers lor Xtypes.mod1Mask; None

      | x when x > 65000 -> None

      (* default *)

      | x -> Some x
    in
    code_opt |> Common.do_option (fun code ->
      let evt = Xtypes.XTKeyPress (!modifiers, GdkEvent.Key.string key, code) in
      (* this will generate a redisplay event via backend.update_display *)
      Top_window.handler top_window evt;
    );
    true
  ) |> ignore;

  win#event#connect#key_release ~callback:(fun key ->
    (match GdkEvent.Key.keyval key with
    | 65507 -> modifiers := !modifiers land (lnot Xtypes.controlMask)
    | 65511 -> modifiers := !modifiers land (lnot Xtypes.mod1Mask)
    | 65513 -> modifiers := !modifiers land (lnot Xtypes.mod1Mask)
    | _ -> ()
    );
    true
  ) |> ignore;

  da#event#connect#button_press (fun ev ->
    let (x, y) = GdkEvent.Button.x ev, GdkEvent.Button.y ev in

    (match GdkEvent.get_type ev with
    | `BUTTON_PRESS ->
        (* TODO should get the latest version *)
        let metrics = metrics in
        let button = GdkEvent.Button.button ev in
        let x = (x / metrics.font_width) |> int_of_float in
        let y = (y / metrics.font_height) |> int_of_float in
        pr2 (spf "click on x = %d, y = %d " x y);
        let evt = Xtypes.XTButtonPress(!modifiers, button, x, y) in
        Top_window.handler top_window evt
    | _ -> ()
    );
    true
  ) |> ignore;

  (*-------------------------------------------------------------------*)
  (* End *)
  (*-------------------------------------------------------------------*)
  let _cursor_thread = start_cursor_thread() in

  let quit () = 
    (*Thread.kill cursor_thread;*)
    GMain.Main.quit (); 
  in

  GtkSignal.user_handler := (fun exn -> 
    (match exn with
    | Common.UnixExit _ -> quit ()
    | _ ->
        pr2 "fucking callback";
        let s = Printexc.get_backtrace () in
        pr2 s;
        (*
          let pb = "pb: " ^ Common.exn_to_s exn in
          G.dialog_text ~text:pb ~title:"pb";
        *)
        raise exn
    )
  );
  win#connect#destroy ~callback:quit |> ignore;
  win#show ();
  GMain.main()


(*****************************************************************************)
(* Test cairo/gtk *)
(*****************************************************************************)

let width = 500
let height = 500

let test_draw_cairo cr =
  (* [0,0][1,1] world scaled to a width x height screen *)
  Cairo.scale cr (float_of_int width) (float_of_int height);

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
  ()

let test_draw_pango cr =

  let layout = Pango_cairo.create_layout cr in
  let ctx = Pango_cairo.FontMap.create_context 
    (Pango_cairo.FontMap.get_default ()) in


  Pango.Layout.set_text layout "WWWWW let x = 1 in main () for x = 1 to 3!";
  let desc = Pango.Font.from_string 
(*
    "Monospace 18"
    "Fixed Bold 32"
    "b&h-Luxi Bold 23"
    "Arial Bold 30" 
    "Menlo 18"
*)
    "Monospace 18"

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
  (* The 'i' should align with the 'W' above if the font is monospace *)
  Pango.Layout.set_text layout "iiiii let x = 1 in main () for x = 1 to 3!";
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
  test_draw_pango cr;
  (GMisc.pixmap px ~packing:w#add ()) |> ignore;
  w#show ();
  GMain.main()

(*****************************************************************************)
(*****************************************************************************)

let init a =
  if !Globals.check
  then test_cairo ()
  else init2 a
