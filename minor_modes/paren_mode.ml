(*s: minor_modes/paren_mode.ml *)
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
open Text
open Simple
open Efuns
open Top_window

(*s: constant Paren_mode.mode *)
let mode = Ebuffer.new_minor_mode "paren" []
(*e: constant Paren_mode.mode *)

(*s: function Paren_mode.find_matching *)
let find_matching  frame = 
  self_insert_command frame; 
  highlight_paren frame
(*e: function Paren_mode.find_matching *)
  
(*s: toplevel Paren_mode._1 *)
let _ = 
  List.iter
    (fun key -> 
      Keymap.add_binding mode.min_map [NormalMap, Char.code key] find_matching
  ) [ ')'; '}'; ']' ]
(*e: toplevel Paren_mode._1 *)

(*s: toplevel Paren_mode._2 *)
let _ = 
  define_buffer_action "paren_mode" 
    (fun buf -> 
      if Ebuffer.modep buf mode 
      then Ebuffer.del_minor_mode buf mode
      else Ebuffer.set_minor_mode buf mode
    )
(*e: toplevel Paren_mode._2 *)
(*e: minor_modes/paren_mode.ml *)
