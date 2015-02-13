(*s: commons2/local.ml *)
(***********************************************************************)
(*                                                                     *)
(*                             ____________                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* We use strings instead of ints to enable migration of datas between
editors (data must be symbolic) *)

open Obj
  
(*s: type Local.var *)
type 'a var = string
(*e: type Local.var *)

(*s: module Local.Vars *)
module Vars = Map.Make 
 (struct  
      type t = string
      let compare = compare 
 end)
(*e: module Local.Vars *)

(*s: type Local.vars *)
(* (string * Obj.t ref) Map ref *)
type vars = Obj.t ref Vars.t ref
(*e: type Local.vars *)
(*s: function Local.vars *)
let vars () = 
  ref Vars.empty
(*e: function Local.vars *)

(*s: global Local.vars_table *)
let (vars_table :
    (string, (Obj.t -> string) * (string -> Obj.t)) Hashtbl.t)
  = Hashtbl.create 203
(*e: global Local.vars_table *)

(*s: function Local.create *)
let create name print input =
  try
    let _ = Hashtbl.find vars_table name in
    failwith (Printf.sprintf "A variable named %s already exists" name)
  with
    Not_found ->
      Hashtbl.add vars_table name (Obj.magic print, Obj.magic input);
      name
(*e: function Local.create *)

(*s: function Local.no_print *)
let no_print _ = "<abstr>"
(*e: function Local.no_print *)
(*s: function Local.no_input *)
let no_input (s : string) = failwith "This variable can not be set"
(*e: function Local.no_input *)
(*s: function Local.create_abstr *)
let create_abstr name = 
  create name no_print no_input
(*e: function Local.create_abstr *)
  
external id : 'a -> 'a = "%identity"

(*s: function Local.create_string *)
let create_string name = 
  create name id id
(*e: function Local.create_string *)
(*s: function Local.create_int *)
let create_int name = 
  create name string_of_int int_of_string
(*e: function Local.create_int *)
(*s: function Local.create_float *)
let create_float name = 
  create name string_of_float float_of_string
(*e: function Local.create_float *)
  
(*s: function Local.get *)
let get vars var =  
  Obj.magic !(Vars.find var !vars)
(*e: function Local.get *)
(*s: function Local.set *)
let set vars var value =
  let value = Obj.repr value in
  try
    let r = Vars.find var !vars in
    r := value
  with
    Not_found -> 
      vars := Vars.add var (ref value) !vars
(*e: function Local.set *)
      
(*s: function Local.get_print *)
let get_print vars var =
  let value = get vars var in
  let (p,i) = Hashtbl.find vars_table var in
  p value  
(*e: function Local.get_print *)
  
(*s: function Local.set_input *)
let set_input vars var value =
  let (p,i) = Hashtbl.find vars_table var in
  set vars var (i value)
(*e: function Local.set_input *)
  
(*s: function Local.list *)
let list vars =
  let list = ref [] in
  Vars.iter (fun var value ->
      list := var :: !list;
  ) !vars;
  !list
(*e: function Local.list *)
  
(*s: constant Local.print *)
let print = get_print
(*e: constant Local.print *)
(*s: constant Local.input *)
let input = set_input
(*e: constant Local.input *)
(*e: commons2/local.ml *)
