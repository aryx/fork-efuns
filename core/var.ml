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

(*s: function Efuns.set_global *)
let set_global var value = 
  Local.set (Globals.location()).loc_vars var value
(*e: function Efuns.set_global *)
(*s: function Efuns.set_local *)
let set_local buf var value = 
  Local.set buf.buf_vars var value
(*e: function Efuns.set_local *)
(*s: function Efuns.get_var *)
let get_var buf var = 
  try 
    Local.get buf.buf_vars var 
  with Not_found ->
    try 
      (*s: [[Efuns.get_var()]] try with major mode variables *)
      Local.get buf.buf_major_mode.maj_vars var
      (*e: [[Efuns.get_var()]] try with major mode variables *)
    with Not_found ->
      try 
        (*s: [[Efuns.get_var()]] try with minor mode variables *)
        let rec iter list =
          match list with
          | [] -> raise Not_found
          | min :: list -> 
              try
                Local.get min.min_vars var
              with _ -> iter list
        in
        iter buf.buf_minor_modes
        (*e: [[Efuns.get_var()]] try with minor mode variables *)
      with Not_found ->
        Local.get (location()).loc_vars var
(*e: function Efuns.get_var *)
          
(*s: function Efuns.get_global *)
let get_global var = 
  Local.get (location()).loc_vars var
(*e: function Efuns.get_global *)
(*s: function Efuns.get_local *)
let get_local buf var = 
  Local.get buf.buf_vars var
(*e: function Efuns.get_local *)
  
(*s: function Efuns.set_minor_var *)
let set_minor_var min var value = 
  Local.set min.min_vars var value
(*e: function Efuns.set_minor_var *)
(*s: function Efuns.set_major_var *)
let set_major_var maj var value = 
  Local.set maj.maj_vars var value
(*e: function Efuns.set_major_var *)
  

(*e: core/var.ml *)
