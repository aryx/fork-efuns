(*s: core/globals.ml *)
(*s: copyright header efuns *)
(***********************************************************************)
(*                                                                     *)
(*                             Efuns                                   *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
(*e: copyright header efuns *)
open Efuns

(* there are other globals: actions, start_hooks, etc *)

(*s: global Efuns.global_location *)
let global_location = ref None
(*e: global Efuns.global_location *)
(*s: function Efuns.location *)
let location () =
  match !global_location with
  | None -> failwith "no global location defined"
  | Some x -> x
(*e: function Efuns.location *)

let with_lock f =
  let loc = location () in
  Mutex.lock loc.loc_mutex;
  Common.finalize f (fun () -> Mutex.unlock loc.loc_mutex)
  
(*s: function Efuns.error *)
let error f x =
  print_string ("error: ");
  Printf.printf f x;
  print_newline ()
(*e: function Efuns.error *)

(*************************************************************************)
(*      Initialization      *)
(*************************************************************************)
  
(* Les variables importantes dans le reste du programme. *)
open Options

(*s: constant Efuns.check *)
let check = ref false
(*e: constant Efuns.check *)

(*s: constants Efuns.debug_xxx *)
let debug = ref false
let debug_graphics = ref false
let debug_display = ref false
let debug_init = ref false
(*e: constants Efuns.debug_xxx *)

(*s: constant Efuns.load_path *)
let load_path = define_option ["efuns_path"] 
  "<load_path> is the path where modules (.cmo and .cma) can be found
  for dynamic linking." path_option []
(*e: constant Efuns.load_path *)

(*s: constant Efuns.path *)
let path = (*Dyneval.load_path*) ref []
(*e: constant Efuns.path *)
  
(*s: constant Efuns.efuns_path *)
let efuns_path = [ 
      (Filename.concat Utils.homedir ".efuns") ;
(*
      Version.efuns_lib; 
      Version.installdir; 
      Version.ocamllib
*)
  ]
(*e: constant Efuns.efuns_path *)
  
(*s: toplevel Efuns._1 *)
let _ = 
  path := !!load_path @ efuns_path;
  option_hook load_path (fun _ -> path := !!load_path @ efuns_path)
(*e: toplevel Efuns._1 *)

(* used in some major mode *)
(*s: constant Efuns.font *)
let font = define_option ["font"] "" string_option "Menlo 18"
(*e: constant Efuns.font *)
  
(*--------------------    Ressources *)
(*s: constant Efuns.xdefaults *)
let xdefaults = try Sys.getenv "XUSERFILESEARCHPATH" with
    Not_found -> Filename.concat Utils.homedir ".Xdefaults"
(*e: constant Efuns.xdefaults *)

(*s: constant Efuns.x_res *)
(*let x_res = Xrm.create ()*)
(*e: constant Efuns.x_res *)
(*s: toplevel Efuns._2 *)
(*
let _ =
  begin    
    try
      let efuns_res = 
        let path = try Utils.string_to_path (Sys.getenv "XFILESEARCHPATH") with _ -> 
              [] in
        let xenv = try Sys.getenv "XENVIRONMENT" with _ -> "" in
        let xroot = try Filename.concat  (Sys.getenv "X11ROOT")
            "lib/X11/app-defaults/" with _ -> "" in
        Utils.find_in_path (path@[
            xenv; xroot; "/usr/X11/lib/X11/app-defaults/"]) "Efuns"
      in
      Xrm.safe_load x_res efuns_res
    with _ -> ()
  end;
  Xrm.safe_load x_res xdefaults
*)
(*e: toplevel Efuns._2 *)
  
(*s: constant Efuns.t *)
(*let t = x_res*)
(*e: constant Efuns.t *)

(*
  let _ = Printf.printf "%d %d %s %s %s" !width !height !font !fg !bg; 
  print_newline () 
*)
  

(*e: core/globals.ml *)