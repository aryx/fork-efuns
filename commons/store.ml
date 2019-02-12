(*s: commons/store.ml *)
(*s: copyright header *)
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
(*e: copyright header *)

(* We use strings instead of ints to enable migration of datas between
editors (data must be symbolic) *)
  
(*s: type [[Store.var]] *)
type 'a var = string
(*e: type [[Store.var]] *)

(*s: module Store.Vars *)
module Vars = Map_
(*e: module Store.Vars *)

(*s: type [[Store.t]] *)
type t = (string, Obj.t ref) Vars.t ref
(*e: type [[Store.t]] *)
(*s: function [[Store.new_store]] *)
let new_store () = 
  ref Vars.empty
(*e: function [[Store.new_store]] *)

(*s: global [[Store.vars_table]] *)
let (vars_table :
    (string, (Obj.t -> string) * (string -> Obj.t)) Hashtbl.t)
  = Hashtbl.create 203
(*e: global [[Store.vars_table]] *)

(*s: function [[Store.create]] *)
let create name print input =
  try
    let _ = Hashtbl.find vars_table name in
    failwith (Printf.sprintf "A variable named %s already exists" name)
  with
    Not_found ->
      Hashtbl.add vars_table name (Obj.magic print, Obj.magic input);
      name
(*e: function [[Store.create]] *)

(*s: function [[Store.no_print]] *)
let no_print x = 
  "<abstr>:" ^ (Common.dump x)
(*e: function [[Store.no_print]] *)
(*s: function [[Store.no_input]] *)
let no_input (_s : string) = failwith "This variable can not be set"
(*e: function [[Store.no_input]] *)
(*s: function [[Store.create_abstr]] *)
let create_abstr name = 
  create name no_print no_input
(*e: function [[Store.create_abstr]] *)
  
external id : 'a -> 'a = "%identity"

(*s: function [[Store.create_string]] *)
let create_string name = 
  create name id id
(*e: function [[Store.create_string]] *)
(*s: function [[Store.create_int]] *)
let create_int name = 
  create name string_of_int int_of_string
(*e: function [[Store.create_int]] *)
(*s: function [[Store.create_float]] *)
let create_float name = 
  create name string_of_float float_of_string
(*e: function [[Store.create_float]] *)
  
(*s: function [[Store.get]] *)
let get vars var =  
  Obj.magic !(Vars.find var !vars)
(*e: function [[Store.get]] *)
(*s: function [[Store.set]] *)
let set vars var value =
  let value = Obj.repr value in
  try
    let r = Vars.find var !vars in
    r := value
  with
    Not_found -> 
      vars := Vars.add var (ref value) !vars
(*e: function [[Store.set]] *)
      
(*s: function [[Store.get_print]] *)
let get_print vars var =
  let value = get vars var in
  let (p, _i) = Hashtbl.find vars_table var in
  p value  
(*e: function [[Store.get_print]] *)
  
(*s: function [[Store.set_input]] *)
let set_input vars var value =
  let (_p, i) = Hashtbl.find vars_table var in
  set vars var (i value)
(*e: function [[Store.set_input]] *)
  
(*s: function [[Store.list]] *)
let list vars =
  let list = ref [] in
  Vars.iter (fun var _value ->
      list := var :: !list;
  ) !vars;
  !list
(*e: function [[Store.list]] *)
  
(*s: constant [[Store.print]] *)
let print = get_print
(*e: constant [[Store.print]] *)
(*s: constant [[Store.input]] *)
let input = set_input
(*e: constant [[Store.input]] *)
(*e: commons/store.ml *)
