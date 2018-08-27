(*s: core/var.ml *)
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
open Globals

(*s: type [[Var.t]] *)
type 'a t = 'a Store.var
(*e: type [[Var.t]] *)

(*s: function [[Var.set_global]] *)
let set_global var value = 
  Store.set (Globals.location()).loc_vars var value
(*e: function [[Var.set_global]] *)
(*s: function [[Var.set_local]] *)
let set_local buf var value = 
  Store.set buf.buf_vars var value
(*e: function [[Var.set_local]] *)
(*s: function [[Var.get_var]] *)
let get_var buf var = 
  try 
    Store.get buf.buf_vars var 
  with Not_found ->
    try 
      (*s: [[Var.get_var()]] try with major mode variables *)
      Store.get buf.buf_major_mode.maj_vars var
      (*e: [[Var.get_var()]] try with major mode variables *)
    with Not_found ->
      try 
        (*s: [[Var.get_var()]] try with minor mode variables *)
        let rec iter list =
          match list with
          | [] -> raise Not_found
          | min :: list -> 
              try
                Store.get min.min_vars var
              with _ -> iter list
        in
        iter buf.buf_minor_modes
        (*e: [[Var.get_var()]] try with minor mode variables *)
      with Not_found ->
        Store.get (location()).loc_vars var
(*e: function [[Var.get_var]] *)
          
(*s: function [[Var.get_global]] *)
let get_global var = 
  Store.get (location()).loc_vars var
(*e: function [[Var.get_global]] *)
(*s: function [[Var.get_local]] *)
let get_local buf var = 
  Store.get buf.buf_vars var
(*e: function [[Var.get_local]] *)
  
(*s: function [[Var.set_minor_var]] *)
let set_minor_var min var value = 
  Store.set min.min_vars var value
(*e: function [[Var.set_minor_var]] *)
(*s: function [[Var.set_major_var]] *)
let set_major_var maj var value = 
  Store.set maj.maj_vars var value
(*e: function [[Var.set_major_var]] *)
  

(*e: core/var.ml *)
