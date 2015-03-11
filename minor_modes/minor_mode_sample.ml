(*s: minor_modes/minor_mode_sample.ml *)
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
          
(*s: constant Minor_mode_sample.minor_mode_name *)
let minor_mode_name = "my_minor"
(*e: constant Minor_mode_sample.minor_mode_name *)

(*s: function Minor_mode_sample.install *)
let install buf = ()
(*e: function Minor_mode_sample.install *)

(*s: function Minor_mode_sample.minor_mode_fun *)
let minor_mode_fun frame = ()
(*e: function Minor_mode_sample.minor_mode_fun *)
  
(*s: constant Minor_mode_sample.mode *)
let mode = Ebuffer.new_minor_mode  minor_mode_name [install]
(*e: constant Minor_mode_sample.mode *)

(*s: toplevel Minor_mode_sample._1 *)
let _ = 
  List.iter
    (fun key -> 
      Keymap.add_binding mode.min_map [NormalMap, Char.code key]
        minor_mode_fun
  )
  [ ] (* no keys *)
(*e: toplevel Minor_mode_sample._1 *)

(*s: toplevel Minor_mode_sample._2 *)
let _ = 
  define_action (minor_mode_name ^ "_mode")
    (fun frame -> 
      let buf = frame.frm_buffer in
      if Ebuffer.modep buf mode 
      then Ebuffer.del_minor_mode buf mode
      else Ebuffer.set_minor_mode buf mode
    )
(*e: toplevel Minor_mode_sample._2 *)
    
(*e: minor_modes/minor_mode_sample.ml *)
