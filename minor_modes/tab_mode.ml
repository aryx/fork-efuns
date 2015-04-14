(*s: minor_modes/tab_mode.ml *)
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
open Simple
open Efuns

(*s: constant Tab_mode.mode *)
let mode = Ebuffer.new_minor_mode "tab" []
(*e: constant Tab_mode.mode *)
  
(*s: function Tab_mode.insert_tab *)
let insert_tab frame = 
  insert_string frame "\t"
(*e: function Tab_mode.insert_tab *)
  
(*s: toplevel Tab_mode._1 *)
let _ = 
  Keymap.add_binding mode.min_map [NormalMap, XK.xk_Tab] insert_tab
(*e: toplevel Tab_mode._1 *)

(*s: toplevel Tab_mode._2 *)
let _ = 
  Action.define_buffer_action "tab_mode" 
    (fun buf -> 
      if Ebuffer.modep buf mode 
      then Ebuffer.del_minor_mode buf mode
      else Ebuffer.set_minor_mode buf mode
  );
  Action.define_action "insert_tab" insert_tab
(*e: toplevel Tab_mode._2 *)
(*e: minor_modes/tab_mode.ml *)
