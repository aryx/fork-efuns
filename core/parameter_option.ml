(*s: core/parameter_option.ml *)
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

(* A parameter is an option that can be interactively get or set.
 * See Interactive.get_parameter and set_parameter.
 *
 * alt: use the Options.options global and make all options parameters?
 * no need then for parameters_var below and the redundant Parameter.t
 *)

(*s: type [[Simple.parameter]] *)
type t = 
 string * ((string -> Obj.t) * (Obj.t -> string) * Obj.t Options.t)
(*e: type [[Simple.parameter]] *)
  
(*s: constant [[Simple.parameters_var]] *)
let parameters_var = Store.create_abstr "parameters"
(*e: constant [[Simple.parameters_var]] *)
  
(*s: function [[Simple.add_parameter]] *)
let add_parameter (name : string) (input : string -> 'a) 
  (print : 'a -> string) (param : 'a Options.t) =
  let (input : string -> Obj.t) = Obj.magic input in
  let (print : Obj.t -> string) = Obj.magic print in
  let (param : Obj.t Options.t) = Obj.magic param in
  Var.set_global parameters_var (
    (name, (input, print, param)) :: 
    (try Var.get_global parameters_var with _ -> []))
(*e: function [[Simple.add_parameter]] *)

(*s: function [[Simple.add_option_parameter]] *)
let add_option_parameter option =
  add_parameter (shortname option)
   (fun s -> from_value (get_type option) (Value s))
   (fun v -> 
      match to_value (get_type option) v with
        Value s -> s
      | _ -> failwith "Unable to print option"
    ) 
    option
(*e: function [[Simple.add_option_parameter]] *)

(* todo: remove useless cache? *)  
(*s: constant [[Simple.all_params]] *)
let all_params = ref None
(*e: constant [[Simple.all_params]] *)
(*s: function [[Simple.all_parameters]] *)
let all_parameters _frame _ =
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

(*e: core/parameter_option.ml *)
