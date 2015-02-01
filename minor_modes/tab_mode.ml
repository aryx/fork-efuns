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

let mode = Ebuffer.new_minor_mode "tab" []
  
let insert_tab frame = ignore (insert_string frame "\t")
  
let _ = 
  Keymap.add_binding mode.min_map [NormalMap, XK.xk_Tab] insert_tab

let _ = 
  define_buffer_action "tab_mode" 
    (fun buf -> 
      if Ebuffer.modep buf mode then begin
          Ebuffer.del_minor_mode buf mode
        end else
        Ebuffer.set_minor_mode buf mode);
  define_action "insert_tab" insert_tab
    
    