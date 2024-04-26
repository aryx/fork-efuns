
type t = 
 string * ((string->Obj.t) * (Obj.t->string) * Obj.t Options.t)

val add_option_parameter : 'a Options.t -> unit

val all_parameters : Efuns.frame -> 'a -> string list

(* internals *)
val parameters_var : t list Var.t

val add_parameter :
  string -> (string -> 'a) -> ('a -> string) -> 'a Options.t -> 
  unit


(*
val add_string_parameter : Efuns.location -> string -> string ref -> unit
val add_int_parameter : Efuns.location -> string -> int ref -> unit
val add_float_parameter : Efuns.location -> string -> float ref -> unit
val add_bool_parameter : Efuns.location -> string -> bool ref -> unit

val all_params : (t list * string list) option ref
*)
