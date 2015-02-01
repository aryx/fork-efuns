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
open Text
open Frame
open Simple
open Select
open Multi_frames  

let compilation_frame = ref None

type error = {
(* error location *)    
    err_filename : string;
    err_line : int;
    err_begin : int;
    err_end : int;
(* error message *)
    err_msg : int;
  }

let c_error_regexp = define_option ["compil"; "error_regexp"] "" regexp_option
  (string_to_regex "^\\([^:\n]+\\):\\([0-9]+\\):.*$")

let c_find_error text error_point =
  let groups = 
    Text.search_forward_groups text (snd !!c_error_regexp)
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

let find_error = Local.create_abstr "find_error"
let default_error = ref c_find_error
  
let next_error top_frame =
  let top_window = Window.top top_frame.frm_window in
  let location = top_window.top_location in
  match !compilation_frame with
    None -> Top_window.message top_window "No compilation started"
  | Some (frame, error_point, cdir) ->      
      if frame.frm_killed then unkill (cut_frame top_frame) frame;
      let buf = frame.frm_buffer in
      let find_error = try
          get_local buf find_error
        with Failure _ -> !default_error
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
          let filename = Filename.concat cdir error.err_filename in
          let buf = Ebuffer.read location filename (Keymap.create ()) in
(* new frame for error buffer *)
          let frame = 
            try find_buffer_frame location buf with Not_found ->
                if frame == top_frame then
                  let display = Window.display top_window in
                  let new_window = 
                    Top_window.create top_window.top_location 
                      display 
                  in
                  Frame.create new_window.top_windows None buf
                else
                  Frame.create top_frame.frm_window None buf
          in
          let text = buf.buf_text in
          let point = frame.frm_point in
          Text.point_to_line text point error.err_line;
          Text.fmove text point error.err_begin;
          Frame.active frame
      with
        Not_found ->
          Top_window.message top_window "No more errors"

let compile_find_makefile = define_option ["compil";"find_makefile"] ""
    bool_option true
  
let make_command = define_option ["compil";"make_command"] ""
    string_option "make -k"
  
let make_hist = ref [!!make_command]
let compile find_error_fun frame =
  let default = List.hd !make_hist in
  select_string frame ("Compile command: (default :"^ default^") " )
  make_hist ""
    (fun cmd -> 
      let cmd = 
        if cmd = "" then default else
          cmd 
      in
      let cdir = Frame.current_dir frame in
      let cdir = 
        if !!compile_find_makefile then
          if String.sub cmd 0 4 = "make" || String.sub cmd 1 4 = "make" then
          (* try to find a Makefile in the directory *)
            let rec iter dir =
              let m = Filename.concat dir "Makefile" in
              if Sys.file_exists m then dir else
              let m = Filename.concat dir "makefile" in
              if Sys.file_exists m then dir else                
              let m = Filename.concat dir "GNUmakefile" in
              if Sys.file_exists m then dir else 
              let newdir = Filename.dirname dir in
              if newdir = dir then cdir else iter newdir
            in
            iter cdir
          else
            cdir
        else cdir
      in
      let top_window = Window.top frame.frm_window in
      let location = top_window.top_location in
      let comp_window =
        match !compilation_frame with
          None -> cut_frame frame 
        | Some (new_frame,error_point, _) ->
            Text.remove_point new_frame.frm_buffer.buf_text error_point;
            Ebuffer.kill location new_frame.frm_buffer;
            if new_frame.frm_killed then cut_frame frame
            else new_frame.frm_window 
      in
      Unix.chdir cdir;
      let comp_frame = System.start_command "*Compile*" comp_window cmd in
      Frame.active frame;
      let buf = comp_frame.frm_buffer in
      let error_point = Text.add_point buf.buf_text in
      compilation_frame := Some (comp_frame, error_point, cdir);
      set_local buf find_error find_error_fun
  )

let set_compilation_buffer frame comp_buf cdir =
  let error_point = add_point comp_buf.buf_text in
  let window =
    match !compilation_frame with
      None -> 
        cut_frame frame
    | Some (frame,point, _) ->
        remove_point frame.frm_buffer.buf_text point;  
        if frame.frm_killed then cut_frame frame 
        else
          frame.frm_window
  in
  let error_point = add_point comp_buf.buf_text in
  let comp_frame = Frame.create window None comp_buf in
  compilation_frame := Some (comp_frame, error_point, cdir)
  
let grep_command = define_option ["compil"; "grep_command"] "" string_option
    "grep -n"
  
let grep_hist = ref [""]
let grep frame =
  let default = List.hd !grep_hist in
  select_string frame (Printf.sprintf "Grep command: %s (default: %s) " !!grep_command default)
  grep_hist ""
    (fun cmd -> 
      let cmd = if cmd = "" then default else cmd in
      let cmd = !!grep_command ^ " " ^ cmd in
      let cdir = Frame.current_dir frame in
      let top_window = Window.top frame.frm_window in
      let location = top_window.top_location in
      let comp_window =
        match !compilation_frame with
          None -> cut_frame frame 
        | Some (new_frame,error_point, _) ->
            Text.remove_point new_frame.frm_buffer.buf_text error_point;
            Ebuffer.kill location new_frame.frm_buffer;
            if new_frame.frm_killed then cut_frame frame
            else new_frame.frm_window 
      in
      Unix.chdir cdir;
      let comp_frame = System.start_command "*Grep*" comp_window cmd in
      Frame.active frame;
      let buf = comp_frame.frm_buffer in
      let error_point = Text.add_point buf.buf_text in
      compilation_frame := Some (comp_frame, error_point, cdir);
      set_local buf find_error c_find_error
  )
