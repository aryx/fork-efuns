(*s: features/compil.ml *)
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
open Common
open Options
open Efuns

(*s: constant Compil.compilation_frame *)
let compilation_frame = ref None
(*e: constant Compil.compilation_frame *)

(*s: type Compil.error *)
type error = {
    (* error location *)    
    err_filename : string;
    err_line : int;
    err_begin : int;
    err_end : int;
    (* error message *)
    err_msg : int;
  }
(*e: type Compil.error *)

type find_error_fun = Text.t -> Text.point -> error

(*s: constant Compil.c_error_regexp *)
let c_error_regexp = define_option ["compil"; "error_regexp"] "" regexp_option
  (string_to_regex "^\\([^:\n]+\\):\\([0-9]+\\):.*$")
(*e: constant Compil.c_error_regexp *)

(*s: function Compil.c_find_error *)
(* todo: vs C_mode.c_find_error? *)
let find_error_gen re text error_point =
  let groups = 
    Text.search_forward_groups text re
      error_point 2 in
  let error =
    {  
      err_msg = Text.get_position text error_point;
      err_filename = groups.(0);
      err_line = (int_of_string groups.(1)) - 1;
      err_begin = 0;
      err_end = 0;
    } in
  Text.fmove text error_point 1;
  error
(*e: function Compil.c_find_error *)

(*s: constant Compil.find_error *)
let find_error = Local.create_abstr "find_error"
(*e: constant Compil.find_error *)

let find_error_location_regexp = Local.create_abstr "find_error_loc_regexp"
let find_error_error_regexp = Local.create_abstr "find_error_err_regexp"
  
(*s: function Compil.next_error *)
let next_error top_frame =
  let top_window = Window.top top_frame.frm_window in
  match !compilation_frame with
  | None -> Top_window.message top_window "No compilation started"
  | Some (frame, error_point, cdir) ->      
      if frame.frm_killed 
      then Frame.unkill (Multi_frames.cut_frame top_frame) frame;

      let buf = frame.frm_buffer in
      let find_error = 
        try Var.get_var buf find_error
        with Not_found | Failure _ -> 
          let re = 
            try Var.get_var buf find_error_location_regexp
            with Not_found | Failure _ ->
              snd !!c_error_regexp
          in
          find_error_gen re
      in
      let text = buf.buf_text in
      let point = frame.frm_point in
      try
        let error = find_error text error_point in
        Text.set_position text frame.frm_start error.err_msg;
        Text.set_position text point error.err_msg;
        frame.frm_force_start <- true;
        frame.frm_redraw <- true;
        if error.err_filename <> "" then
          let filename = 
            if error.err_filename =~ "^/"
            then error.err_filename
            else Filename.concat cdir error.err_filename 
          in
          let buf = Ebuffer.read filename (Keymap.create ()) in
          (* new frame for error buffer *)
          let frame = 
            try Frame.find_buffer_frame buf 
            with Not_found ->
                if frame == top_frame then
                  let new_window = Top_window.create ()
                      (*Window.display top_window*) 
                  in
                  Frame.create new_window.window None buf
                else
                  Frame.create top_frame.frm_window None buf
          in
          let text = buf.buf_text in
          let point = frame.frm_point in
          Text.point_to_line text point error.err_line;
          Text.fmove text point error.err_begin;
          Frame.active frame
      with Not_found ->
        Top_window.message top_window "No more errors"
(*e: function Compil.next_error *)

(*s: constant Compil.compile_find_makefile *)
let compile_find_makefile = define_option ["compil";"find_makefile"] ""
    bool_option true
(*e: constant Compil.compile_find_makefile *)
  
(*s: constant Compil.make_command *)
let make_command = define_option ["compil";"make_command"] ""
    string_option "make -k"
(*e: constant Compil.make_command *)

let color_buffer buf =
  Dircolors.colorize buf;

  let re =
    try Var.get_var buf find_error_location_regexp
    with Not_found | Failure _ -> snd !!c_error_regexp
  in
  Simple.color buf re false
    (Text.make_attr (Attr.get_color "green") 1 0 false);

  let re =
    try Var.get_var buf find_error_error_regexp
    with Not_found | Failure _ -> Str.regexp "Error"
  in
  Simple.color buf re false
    (Text.make_attr (Attr.get_color "red") 1 0 false);
  ()

let install buf =
  color_buffer buf

let mode = Ebuffer.new_major_mode "Compilation" [install]
  
(*s: constant Compil.make_hist *)
let make_hist = ref [!!make_command]
(*e: constant Compil.make_hist *)
(*s: function Compil.compile *)
let compile frame =
  let default = List.hd !make_hist in
  Select.select_string frame 
    ("Compile command: (default :"^ default^") " )
    make_hist ""
    (fun cmd -> 
      let cmd = if cmd = "" then default else cmd in (* cmd ||| default *)
      let cdir = Frame.current_dir frame in
      (*s: [[Compil.compile()]] find possibly cdir with a makefile *)
      let cdir = 
        if !!compile_find_makefile then
          if String.sub cmd 0 4 = "make" || String.sub cmd 1 4 = "make" then
          (* try to find a Makefile in the directory *)
            let rec iter dir =
              if Sys.file_exists (Filename.concat dir "Makefile") ||
                 Sys.file_exists (Filename.concat dir "makefile") ||
                 Sys.file_exists (Filename.concat dir "GNUmakefile")
              then dir 
              else
                let newdir = Filename.dirname dir in
                if newdir = dir 
                then cdir 
                else iter newdir
            in
            iter cdir
          else cdir
        else cdir
      in
      (*e: [[Compil.compile()]] find possibly cdir with a makefile *)
      let comp_window =
        match !compilation_frame with
        | None -> Multi_frames.cut_frame frame 
        | Some (new_frame,error_point, _) ->
            Text.remove_point new_frame.frm_buffer.buf_text error_point;
            Ebuffer.kill new_frame.frm_buffer;
            if new_frame.frm_killed 
            then Multi_frames.cut_frame frame
            else new_frame.frm_window 
      in
      let comp_frame = 
        System.start_command cdir "*Compile*" comp_window cmd 
        (Some (fun buf _status -> color_buffer buf))
      in
      Frame.active frame;
      let buf = comp_frame.frm_buffer in
      let error_point = Text.new_point buf.buf_text in
      compilation_frame := Some (comp_frame, error_point, cdir);

      (* propagate vars *)
      let buf2 = frame.frm_buffer in
      (try 
         let x = Var.get_var buf2 find_error in
         Var.set_local buf find_error x
       with Not_found | Failure _ -> ()
      );
      (try 
         let x =  Var.get_var buf2 find_error_location_regexp in
         Var.set_local buf find_error_location_regexp x
       with Not_found | Failure _ -> ()
      );
      (try 
         let x =  Var.get_var buf2 find_error_error_regexp in
         Var.set_local buf find_error_error_regexp x
       with Not_found | Failure _ -> ()
      );
      Ebuffer.set_major_mode buf mode

  )
(*e: function Compil.compile *)

(*s: constant Compil.grep_command *)
let grep_command = define_option ["compil"; "grep_command"] "" string_option
    "grep -n"
(*e: constant Compil.grep_command *)
  
(*s: constant Compil.grep_hist *)
let grep_hist = ref [""]
(*e: constant Compil.grep_hist *)
(*s: function Compil.grep *)
let grep frame =
  let default = List.hd !grep_hist in
  Select.select_string frame 
    (Printf.sprintf "Grep command: %s (default: %s) " !!grep_command default)
    grep_hist ""
    (fun cmd -> 
      let cmd = if cmd = "" then default else cmd in
      let cmd = !!grep_command ^ " " ^ cmd in
      let cdir = Frame.current_dir frame in
      let comp_window =
        match !compilation_frame with
        | None -> Multi_frames.cut_frame frame 
        | Some (new_frame,error_point, _) ->
            Text.remove_point new_frame.frm_buffer.buf_text error_point;
            Ebuffer.kill new_frame.frm_buffer;
            if new_frame.frm_killed 
            then Multi_frames.cut_frame frame
            else new_frame.frm_window 
      in
      let comp_frame = System.start_command cdir "*Grep*" comp_window cmd None
      in
      Frame.active frame;
      let buf = comp_frame.frm_buffer in
      let error_point = Text.new_point buf.buf_text in
      compilation_frame := Some (comp_frame, error_point, cdir)
  )
(*e: function Compil.grep *)
(*e: features/compil.ml *)
