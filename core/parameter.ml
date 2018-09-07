(*s: features/parameter.ml *)
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
open Options

(*s: type [[Simple.parameter]] *)
type parameter = (string * ((string -> Obj.t) * (Obj.t -> string) * 
      Obj.t option_record))
(*e: type [[Simple.parameter]] *)
  
(*s: constant [[Simple.parameters_var]] *)
let parameters_var = Store.create_abstr "parameters"
(*e: constant [[Simple.parameters_var]] *)
  
(*s: function [[Simple.add_parameter]] *)
let add_parameter (name : string) (input : string -> 'a) 
  (print : 'a -> string) (param : 'a option_record) =
  let (input : string -> Obj.t) = Obj.magic input in
  let (print : Obj.t -> string) = Obj.magic print in
  let (param : Obj.t option_record) = Obj.magic param in
  Var.set_global parameters_var (
    (name, (input, print, param)) :: 
    (try Var.get_global parameters_var with _ -> []))
(*e: function [[Simple.add_parameter]] *)

(*s: function [[Simple.add_option_parameter]] *)
let add_option_parameter option =
  add_parameter (shortname option)
   (fun s -> from_value (get_class option) (Value s))
   (fun v -> 
      match to_value (get_class option) v with
        Value s -> s
      | _ -> failwith "Unable to print option"
    ) 
    option
(*e: function [[Simple.add_option_parameter]] *)
  
(*s: constant [[Simple.all_params]] *)
let all_params = ref None
(*e: constant [[Simple.all_params]] *)
(*s: function [[Simple.all_parameters]] *)
let all_parameters frame _ =
  let parameters = 
    try Var.get_global parameters_var with _ -> []
  in
  match !all_params with
    Some (f,l) when f == parameters -> l
  | _ ->
      let list = List.map fst parameters in
      all_params := Some (parameters, list);
      list
(*e: function [[Simple.all_parameters]] *)

(*e: features/parameter.ml *)
