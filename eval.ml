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
open Text
open Frame
open Simple
open Select
open Search
open Compil

  (*
let rec load_module mod_name =
  let filename = (String.uncapitalize mod_name) ^ ".cmo" in
  try
    let filename = Utils.find_in_path !Efuns.load_path filename in
    load_file filename
  with
    Not_found -> failwith ("Can't find file "^filename)

and load_file filename =
  try
    Dynlink.loadfile filename
  with
    Dynlink.Error (Dynlink.Unavailable_unit mod_name) -> 
      load_interface mod_name;
      load_file filename
  | Dynlink.Error (
    Dynlink.Linking_error (_, Dynlink.Undefined_global mod_name)) ->
      load_module mod_name;
      load_file filename

and load_interface mod_name =
  try
    Dynlink.add_interfaces [mod_name] !Efuns.load_path; ()
  with
    Not_found ->
      failwith (Printf.sprintf "No interface for %s" mod_name)
      *)

open Dyneval

let try_load top_window f =
  try
    f (); 
    Efuns.init top_window.top_location
  with
    Dynlink.Error error ->
      Top_window.message top_window (Dynlink.error_message error)
  | e -> 
      Top_window.message top_window
        (Printf.sprintf "Exception %s" (Printexc.to_string e))

let load top_window mod_name =
  try_load top_window (fun () -> load_module mod_name)

let load_library frame =
  select_lib_filename frame "Load library: " 
    (fun str ->
      let top_window = Window.top frame.frm_window in
      try_load top_window (fun () -> load_file str))

  
let eval_buffer frame =
  let top_window = Window.top frame.frm_window in
  let location = top_window.top_location in
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let filename = Filename.temp_file "buffer" ".ml" in
  let outc = open_out filename in
  Text.save buf.buf_text outc;
  close_out outc;
  let cmd = "ocamlc -c -I " ^ Version.efuns_lib ^ " " ^ filename in 
  let end_action buf status =
    match status with
      0 -> try_load top_window 
          (fun _ -> 
            let fl =  ((Filename.chop_suffix filename ".ml") ^ ".cmo") 
            in 
            load_file fl;
            Efuns.init top_window.top_location)
    | _ -> 
        set_compilation_buffer frame buf (Filename.dirname filename);
        Top_window.message top_window "Error while compiling buffer"
  in
  let _ = System.system "*Eval*" location cmd end_action in ()
