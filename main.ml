(*s: main.ml *)
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

open Options

open Efuns
  
(*s: constant Main.location *)
let location = {
    loc_buffers = Hashtbl.create 13;
    loc_files = Hashtbl.create 13;

    loc_windows = [];

    loc_width = !!width;
    loc_height = !!height;

    loc_fg = !!foreground;
    loc_bg = !!background;
    loc_font = !!font;

    loc_map = Keymap.create ();
    loc_dirname = Sys.getcwd ();

    loc_vars = Local.vars ();
    loc_counter = 0;
    
    loc_fonts = Hashtbl.create 37;
    loc_fonts_names = Array.create 256 "";
    loc_fonts_n = 0;

    loc_colors = Hashtbl.create 37;
    loc_colors_names = Array.create 256 "";
    loc_colors_n = 0;
    
    loc_mutex = Mutex.create ()
} 
(*e: constant Main.location *)

(*s: exception Main.SigInt *)
exception SigInt
(*e: exception Main.SigInt *)
(*s: toplevel Main._1 *)
let _ =
  Utils.register_exn (fun e ->
      match e with
        Unix.Unix_error (error,f,arg) ->
          Printf.sprintf "Unix error %s: %s %s" 
            f (Unix.error_message error) arg
      | _ -> raise e);

  Utils.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->  
        (* Should auto-save all buffers, and then exit ... *)
        exit 1)); 
  Utils.set_signal Sys.sigterm (Sys.Signal_handle 
      (fun _ ->  
        (* Should auto-save all buffers, and then exit ... *)
        raise SigInt));
  Utils.set_signal Sys.sighup (Sys.Signal_handle (fun _ ->  raise SigInt))
(*e: toplevel Main._1 *)

(*s: constant Main.highlight_color *)
let highlight_color = define_option ["highlight_color"] "" color_option "cyan"
(*e: constant Main.highlight_color *)
  
(*s: toplevel Main._2 *)
let _ =
(* color 0 is foreground *)
  let _ = Window.get_color location !!foreground in
(* color 1 is background *)
  let _ = Window.get_color location !!background in
(* color 2 is highlight *)
  let _ = Window.get_color location !!highlight_color in
(*x: toplevel Main._2 *)
(* font 0 is initial font *)
  Window.get_font location !!font
(*e: toplevel Main._2 *)

(*s: toplevel Main._3 *)
let _ =
  Efuns.init location (* launch first hooks *)
(*e: toplevel Main._3 *)
  
let _ =
  Graphics_graphics.init location !displayname

(*e: main.ml *)
