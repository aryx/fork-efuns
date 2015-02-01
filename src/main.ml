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
open Obj

open Text
open Efuns
  
let location = {
    loc_map = Keymap.create ();
    loc_windows = [];
    loc_buffers = Hashtbl.create 13;
    loc_files = Hashtbl.create 13;
    loc_dirname = Sys.getcwd ();
    loc_width = !!width;
    loc_height = !!height;
    loc_fg = !!foreground;
    loc_bg = !!background;
    loc_font = !!font;
    loc_vars = Local.vars ();
    loc_counter = 0;
    
    loc_fonts = Hashtbl.create 37;
    loc_fonts_names = Array.create 256 "";
    loc_fonts_n = 0;
    loc_colors = Hashtbl.create 37;
    loc_colors_names = Array.create 256 "";
    loc_colors_n = 0;
    
    loc_mutex = Concur.Mutex.create ()
} 

exception SigInt
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

let highlight_color = define_option ["highlight_color"] ""
    color_option "cyan"
  
let _ =
(* color 0 is foreground *)
  let _ = Window.get_color location !!foreground in
(* color 1 is background *)
  let _ = Window.get_color location !!background in
(* color 2 is highlight *)
  let _ = Window.get_color location !!highlight_color in
(* font 0 is initial font *)
  Window.get_font location !!font

let _ =
  Efuns.init location (* launch first hooks *)
  
  
let dpy_oo = new WX_display.t !displayname
let root_oo = new WX_root.t dpy_oo 0
let display = WX_xterm.create_display root_oo
  location.loc_colors_names location.loc_fonts_names 
let top_window = Top_window.create location display
  
let _ =
  WX_xterm.setHighlight display 2;
  Dyneval.init true;
  Eval.load top_window "Efunsrc";
  Efuns.init location; (* launch second hooks *)
  let _ = Interactive.create_bindings location in

(* open the fisrt buffers *)
  List.iter (fun name ->
    let _ = Frame.load_file top_window.top_windows name in ()) 
  !init_files;
  List.iter 
    (fun str -> let top_window = Top_window.create top_window.top_location
          (Window.display top_window) in
      let _ = Frame.load_file top_window.top_windows str in ()) !init_frames;
  Top_window.update_display location;

  
  if not (Sys.file_exists (Filename.concat Utils.homedir ".efunsrc")) then
    begin
      Printf.printf "Saving .efunsrc after install"; print_newline ();
      Options.save ();
    end;

  if !check then exit 0;  
  (* Main loop *)
  let rec loop () =
    try
      WX_types.loop ()
    with
      SigInt -> loop ()
  in
  loop ()

  



  
