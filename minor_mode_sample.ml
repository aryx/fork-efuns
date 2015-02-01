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

open Text
open Simple
open Efuns
open Top_window
          
let minor_mode_name = "my_minor_mode"

let install buf = ()

let minor_mode_fun frame = ()
  
let mode = Ebuffer.new_minor_mode  minor_mode_name [install]

let _ = 
  List.iter
    (fun key -> 
      Keymap.add_binding mode.min_map [NormalMap, Char.code key] 
      minor_mode_fun
  )
  [ ]

let _ = 
  define_action (minor_mode_name ^ "_mode")
    (fun frame -> 
      let buf = frame.frm_buffer in
      if Ebuffer.modep buf mode then begin
          Ebuffer.del_minor_mode buf mode
        end else
        Ebuffer.set_minor_mode buf mode)
    
    