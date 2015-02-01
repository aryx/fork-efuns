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

let mode = Ebuffer.new_minor_mode "paren" []

let find_matching  frame = self_insert_command frame; highlight_paren frame
  
let _ = 
  List.iter
    (fun key -> 
      Keymap.add_binding mode.min_map [NormalMap, Char.code key] find_matching
  ) [ ')'; '}'; ']' ]

let _ = 
  define_buffer_action "paren_mode" 
    (fun buf -> 
      if Ebuffer.modep buf mode then begin
          Ebuffer.del_minor_mode buf mode
        end else
        Ebuffer.set_minor_mode buf mode)
    
    