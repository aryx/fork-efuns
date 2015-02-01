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
open Text
open Simple
open Efuns
open Top_window
          
let minor_mode_name = "fill"
  
let install buf = ()

let fill_line_len = define_option ["fill_line_len"] "" int_option 80
  
let mode = Ebuffer.new_minor_mode minor_mode_name [install]

let fill_on_char = define_option ["fill_on_char"] "" string_option " "
  
let _ =
  let fill_on_char = !!fill_on_char in
  for i = 0 to String.length fill_on_char - 1 do 
      Keymap.add_binding mode.min_map [NormalMap, Char.code fill_on_char.[i]] 
        electric_insert_space
  done
  
let _ = 
  define_buffer_action (minor_mode_name ^ "_mode")
    (fun buf -> 
      if Ebuffer.modep buf mode then begin
          Ebuffer.del_minor_mode buf mode
        end else
        Ebuffer.set_minor_mode buf mode);
    
    