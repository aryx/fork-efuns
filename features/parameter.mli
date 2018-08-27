
type parameter =    (string * ((string -> Obj.t) * (Obj.t -> string) * 
      Obj.t Options.option_record))
val parameters_var : parameter list Store.var
val add_parameter :
  string -> (string -> 'a) -> ('a -> string) -> 'a Options.option_record -> unit

val add_option_parameter : 'a Options.option_record -> unit
val all_params : (parameter list * string list) option ref
val all_parameters : Efuns.frame -> 'a -> string list
(*
external id : 'a -> 'a = "%identity"
val add_string_parameter : Efuns.location -> string -> string ref -> unit
val add_int_parameter : Efuns.location -> string -> int ref -> unit
val add_float_parameter : Efuns.location -> string -> float ref -> unit
val add_bool_parameter : Efuns.location -> string -> bool ref -> unit
*)
