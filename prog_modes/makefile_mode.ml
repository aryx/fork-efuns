(*s: prog_modes/makefile_mode.ml *)
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
open Keymap
open Efuns
open Simple
open Compil
open Complex
open Window

(*s: constant Makefile_mode.mkfile_vars *)
let mkfile_vars= Str.regexp "\\(\\$([a-zA-Z0-9_]*)\\)\\|\\([a-zA-Z0-9_]+=\\)"
(*e: constant Makefile_mode.mkfile_vars *)
(*s: constant Makefile_mode.mkfile_target *)
let mkfile_target= Str.regexp "^.*:"
(*e: constant Makefile_mode.mkfile_target *)
(*s: constant Makefile_mode.mkfile_rules *)
let mkfile_rules= Str.regexp "^\t.*$"
(*e: constant Makefile_mode.mkfile_rules *)

(*s: constant Makefile_mode.rules_color *)
let rules_color = define_option ["makefile_mode";"rules_color"] "" 
  string_option "red"
(*e: constant Makefile_mode.rules_color *)
(*s: constant Makefile_mode.target_color *)
let target_color = define_option ["makefile_mode"; "target_color"] ""
    string_option "cadetblue"
(*e: constant Makefile_mode.target_color *)
(*s: constant Makefile_mode.vars_color *)
let vars_color = define_option ["makefile_mode"; "vars_color"] ""
    string_option "blue"
(*e: constant Makefile_mode.vars_color *)
  
(*s: function Makefile_mode.makefile_color *)
let makefile_color buf =
  let location = buf.buf_location in
  color buf mkfile_rules false
    (make_attr (get_color location !!rules_color) 1 0 false);
  color buf mkfile_target false 
    (make_attr (get_color location !!target_color) 1 0 false);
  color buf mkfile_vars false 
    (make_attr (get_color location !!vars_color) 1 0 false)
(*e: function Makefile_mode.makefile_color *)
 
(*s: constant Makefile_mode.c_c *)
let c_c = (ControlMap,Char.code 'c')
(*e: constant Makefile_mode.c_c *)

(*s: function Makefile_mode.install *)
let install buf =
  makefile_color buf;
  execute_buffer_action "tab_mode" buf
(*e: function Makefile_mode.install *)
  
(*s: constant Makefile_mode.mode *)
let mode = Ebuffer.new_major_mode "Makefile" [makefile_color]
(*e: constant Makefile_mode.mode *)
  

(*s: constant Makefile_mode.local_map *)
let local_map = define_option ["makefile_mode"; "local_map"] ""
    (list_option binding_option) []
(*e: constant Makefile_mode.local_map *)

(*s: constant Makefile_mode.interactives_map *)
let interactives_map = define_option ["makefile_mode"; "interactives_map"] ""
    (list_option string2_option) 
  []
(*e: constant Makefile_mode.interactives_map *)

(*s: toplevel Makefile_mode._1 *)
(* let insert_tab frame = ignore (insert_string frame "\t") *)
  
let _ =
  if !!local_map = [] then
    local_map =:= [
      [c_c; ControlMap, Char.code 'c'], "makefile_mode.compile";    
      [ControlMap, Char.code 'l'], "makefile_mode.color_buffer";
     (*  [NormalMap, XK.xk_Tab], "insert_tab"; *)
    ];
  if !!interactives_map = [] then 
        interactives_map =:= [
          "compile", "makefile_mode.compile";
          "color_buffer", "makefile_mode.color_buffer";
      ]
(*e: toplevel Makefile_mode._1 *)

(*s: function Makefile_mode.makefile_mode *)
let makefile_mode frame = Ebuffer.set_major_mode frame.frm_buffer mode
(*e: function Makefile_mode.makefile_mode *)
        
(*s: toplevel Makefile_mode._2 *)
let _ = 
  define_action "makefile_mode.compile" (compile c_find_error);
  define_action "makefile_mode.color_buffer" 
    (fun frame -> makefile_color frame.frm_buffer);
(*  define_action "insert_tab" insert_tab; *)
  define_action "makefile_mode" makefile_mode;
  ()
(*e: toplevel Makefile_mode._2 *)

(*s: toplevel Makefile_mode._3 *)
let _ =
  let map = mode.maj_map in
  List.iter (fun (keys, action) ->
      try
        Keymap.add_binding map keys (execute_action action)
      with e ->
          Log.printf "Error for action %s" action;
          Log.exn "%s\n" e;
  
  ) !!local_map;
  List.iter (fun (name, action) ->
      try
        add_interactive map name (execute_action action)
      with e ->
          Log.printf "Error for action %s" action;
          Log.exn "%s\n" e;
          
  ) !!interactives_map;
  ()
(*e: toplevel Makefile_mode._3 *)

  
(*s: toplevel Makefile_mode._4 *)
let _ = 
  (* Keymap.add_prefix mode.maj_map [c_c];   *)
  Efuns.add_start_hook (fun location ->
      let alist = get_global (location) Ebuffer.modes_alist in
      set_global location Ebuffer.modes_alist 
        ((".*/[Mm]akefile.*",mode)
        :: alist);
      add_option_parameter location vars_color;
      add_option_parameter location target_color;
      add_option_parameter location rules_color;
      )   
(*e: toplevel Makefile_mode._4 *)
(*e: prog_modes/makefile_mode.ml *)
