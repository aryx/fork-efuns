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
  
type 'a var = string

module Vars = Map.Make (struct  
      type t = string
      let compare = compare end)

type vars = Obj.t ref Vars.t ref
let vars () = ref Vars.empty

let (vars_table :
    (string, (Obj.t -> string) * (string -> Obj.t)) Hashtbl.t)
  = Hashtbl.create 203

let create name print input =
  try
    let _ = Hashtbl.find vars_table name in
    failwith (Printf.sprintf "A variable named %s already exists" name)
  with
    Not_found ->
      Hashtbl.add vars_table name (magic print,magic input);
      name

let no_print _ = "<abstr>"
let no_input (s : string) = failwith "This variable can not be set"
let create_abstr name = create name no_print no_input
  
external id : 'a -> 'a = "%identity"

let create_string name = create name id id
let create_int name = create name string_of_int int_of_string
let create_float name = create name string_of_float float_of_string
  
let get vars var =  Obj.magic !(Vars.find var !vars)
let set vars var value =
  let value = repr value in
  try
    let r = Vars.find var !vars in
    r := value
  with
    Not_found -> 
      vars := Vars.add var (ref value) !vars
      
let get_print vars var =
  let value = get vars var in
  let (p,i) = Hashtbl.find vars_table var in
  p value  
  
let set_input vars var value =
  let (p,i) = Hashtbl.find vars_table var in
  set vars var (i value)
  
let list vars =
  let list = ref [] in
  Vars.iter (fun var value ->
      list := var :: !list;
  ) !vars;
  !list
  
let print = get_print
let input = set_input