(*s: main.ml *)
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
open Options
open Efuns

(*s: constant Efuns.init_files *)
let init_files = ref []
(*e: constant Efuns.init_files *)
(*s: constant Efuns.init_frames *)
let init_frames = ref []
(*e: constant Efuns.init_frames *)
(*s: constant Efuns.displayname *)
let displayname = ref ""
(*e: constant Efuns.displayname *)
(*s: constant Efuns.no_init *)
let no_init = ref false
(*e: constant Efuns.no_init *)

(*s: function Efuns.init *)
let init_efuns (location : location) =
  global_location := Some location;
  let rec iter hooks =
    match hooks with
      [] -> ()
    | (f : unit -> unit) :: hooks -> 
        f ();
        iter hooks
  in
  let hooks = List.rev !start_hooks in
  start_hooks := [];
  iter hooks
(*e: function Efuns.init *)

(*--------------------    Arguments *)
(*s: constant Efuns.width_opt *)
let width_opt = ref None
(*e: constant Efuns.width_opt *)
(*s: constant Efuns.height_opt *)
let height_opt = ref None
(*e: constant Efuns.height_opt *)
(*s: constant Efuns.font_opt *)
let font_opt = ref None
(*e: constant Efuns.font_opt *)
(*s: constant Efuns.fg_opt *)
let fg_opt = ref None
(*e: constant Efuns.fg_opt *)
(*s: constant Efuns.bg_opt *)
let bg_opt = ref None
(*e: constant Efuns.bg_opt *)
  
(*s: constant Efuns.usage_str *)
let usage_str =
 "A small editor entirely written in Objective Caml 
by Fabrice LE FESSANT, INRIA Rocquencourt, FRANCE
http://pauillac.inria.fr/efuns
Options:
"
(*e: constant Efuns.usage_str *)

(*s: toplevel Efuns._3 *)
let _ =
 Arg.parse [
   (*s: [[main()]] command line options *)
   "-width", Arg.Int (fun i -> width_opt := Some i), "<len>: Width in chars";
   "-height", Arg.Int (fun i -> height_opt := Some i), "<len>: Height in chars";
   "-fg", Arg.String(fun s -> fg_opt :=Some s), "<color>: Foreground color";
   "-bg", Arg.String(fun s -> bg_opt :=Some s), "<color>: Background color";
   "-font", Arg.String(fun s -> font_opt :=Some s), "<font>: Font name";
   (*x: [[main()]] command line options *)
   "-d", Arg.String(fun s -> displayname := s),"<dpy>: Name of display";
   "--display", Arg.String(fun s -> displayname := s),"<dpy>: Name of display";
   (*x: [[main()]] command line options *)
   "-check", Arg.Set check, ": only for testing";
   (*x: [[main()]] command line options *)
     "-q", Arg.Set no_init,": Don't load init files";
   (*x: [[main()]] command line options *)
     "-I",Arg.String (fun s -> load_path =:= 
         (Utils.string_to_path s) @ !!load_path), "<path>: Load Path";
   (*x: [[main()]] command line options *)
     "-frame", Arg.String (fun s -> init_frames := s:: !init_frames), "<file>: open a frame with <file>";
   (*e: [[main()]] command line options *)
   "-debug", Arg.Set Efuns.debug, 
   " for debugging";
   "-debug_graphics", Arg.Set Efuns.debug_graphics, 
   " for debugging";
   "-debug_display", Arg.Set Efuns.debug_display, 
   " for debugging";
   "-debug_init", Arg.Set Efuns.debug_init, 
   " for debugging";

   "-debugger", Arg.Unit (fun () ->
     Efuns.debug := true;
   ), " for debugging";

 ]
 (fun name -> init_files := name :: !init_files)
 usage_str
(*e: toplevel Efuns._3 *)
(*s: toplevel Efuns._4 *)
let _ =
  Options.filename := 
   (try Utils.find_in_path (Utils.homedir :: !!load_path) ".efunsrc" 
    with _ -> Filename.concat Utils.homedir ".efunsrc");
  (try Options.init () with _ -> ())
(*e: toplevel Efuns._4 *)

open Options
  
(*s: constant Efuns.width *)
let width = define_option ["width"] "" int_option 80
(*e: constant Efuns.width *)
(*s: constant Efuns.height (core/efuns.ml) *)
let height = define_option ["height"] "" int_option 25
(*e: constant Efuns.height (core/efuns.ml) *)
(*s: constant Efuns.font *)
let font = define_option ["font"] "" string_option "fixed"
(*e: constant Efuns.font *)
(*s: constant Efuns.foreground *)
let foreground = define_option ["foreground"] "" string_option "wheat"
(*e: constant Efuns.foreground *)
(*s: constant Efuns.background *)
let background = define_option ["background"] "" string_option "DarkSlateGray"
(*e: constant Efuns.background *)

(*s: constant Main.highlight_color *)
let highlight_color = define_option ["highlight_color"] "" color_option "cyan"
(*e: constant Main.highlight_color *)
  
  
(*s: toplevel Efuns._5 *)
let _ =
  (match !width_opt with None -> () | Some color -> width =:= color);
  (match !height_opt with None -> () | Some color -> height =:= color);
  (match !fg_opt with None -> () | Some color -> foreground =:= color);
  (match !bg_opt with None -> () | Some color -> background =:= color);
  (match !font_opt with None -> () | Some color -> font =:= color)
(*e: toplevel Efuns._5 *)
  
(*s: constant Main.location *)
let location = {
    loc_buffers = Hashtbl.create 13;
    loc_files = Hashtbl.create 13;

    top_windows = [];

    loc_width = !!width;
    loc_height = !!height;
    loc_fg = !!foreground;
    loc_bg = !!background;
    loc_font = !!font;

    loc_map = Keymap.create ();
    loc_dirname = Sys.getcwd ();

    loc_vars = Local.vars ();
    
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
  init_efuns location (* launch first hooks *)
(*e: toplevel Main._3 *)

(*s: toplevel Main._4 *)
let _ =
  Graphics_efuns.init location !init_files
(*e: toplevel Main._4 *)
(*e: main.ml *)
