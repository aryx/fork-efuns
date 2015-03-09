(***********************************************************************)
(*                                                                     *)
(*                             ____                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

type 'a option_class
type 'a option_record

val filename : string ref
val define_option :
  string list ->  string -> 'a option_class -> 'a -> 'a option_record

val ( !! ) : 'a option_record -> 'a
val ( =:= ) : 'a option_record -> 'a -> unit
val option_hook : 'a option_record -> (unit -> unit) -> unit
val class_hook : 'a option_class -> ('a option_record -> unit) -> unit
  
val save : unit -> unit
val save_with_help : unit -> unit
(*:
val load : unit -> unit
val append : string -> unit
*)

val shortname : 'a option_record -> string
val get_class : 'a option_record -> 'a option_class
val get_help : 'a option_record -> string  
val init : unit -> unit
  
  (*** To create your own options classes ... *)
  
type option_value =
  Module of option_module
| Value of  string
| List of option_value list
| SmallList of option_value list
  
and option_module =
  (string * option_value) list

val define_option_class :
  string -> (option_value -> 'a) -> ('a -> option_value) -> 'a option_class

val to_value : 'a option_class -> 'a -> option_value
val from_value : 'a option_class -> option_value -> 'a
  
val string_option : string option_class
val color_option : string option_class
val font_option : string option_class
val int_option : int option_class
val bool_option : bool option_class
val float_option : float option_class
val path_option : string list option_class
val string2_option : (string * string) option_class
val filename_option : string option_class
  
  (* parameterized options *)
val list_option : 'a option_class -> 'a list option_class
val smalllist_option : 'a option_class -> 'a list option_class
val sum_option : (string * 'a) list -> 'a option_class
val tuple2_option :  
  'a option_class * 'b option_class -> ('a * 'b) option_class
val tuple3_option : 'a option_class * 'b option_class * 'c option_class ->
  ('a * 'b * 'c) option_class
  
val value_to_string : option_value -> string
val string_to_value : string -> option_value
val value_to_int : option_value -> int
val int_to_value : int -> option_value
val bool_of_string : string -> bool
val value_to_bool : option_value -> bool
val bool_to_value : bool -> option_value
val value_to_float : option_value -> float
val float_to_value : float -> option_value
val value_to_string2 : option_value -> string * string
val string2_to_value : string * string -> option_value
val value_to_list : (option_value -> 'a) -> option_value -> 'a list
val list_to_value : ('a -> option_value) -> 'a list -> option_value
val smalllist_to_value : ('a -> option_value) -> 'a list -> option_value
val value_to_path : option_value -> string list
val path_to_value : string list -> option_value

val help : out_channel -> unit
