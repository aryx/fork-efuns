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

(* example of use:
 * foo.ml:
 *  open Options
 *  let width      = define_option ["width"] "" int_option 80
 *  let foreground = define_option ["foreground"] "" string_option "wheat"
 *  ...
 *  let foo () = 
 *    let w = !!width in
 *    ...
 * main.ml:
 *  let main () =
 *    Options.filename := "/home/pad/.config";
 *    Options.init ();
 *    ...
 *)

type 'a type_
type 'a t

val define_option :
  string list (*path*) -> string (*help*) -> 'a type_ -> 'a (*default*) ->
  'a t

val ( !! ) : 'a t -> 'a
val ( =:= ) : 'a t -> 'a -> unit
val shortname : 'a t -> string
val get_type : 'a t -> 'a type_
val get_help : 'a t -> string  

val bool_option     : bool type_
val int_option      : int type_
val float_option    : float type_
val string_option   : string type_

val color_option    : string type_
val font_option     : string type_
val path_option     : string list type_
val string2_option  : (string * string) type_
val filename_option : string type_

val filename : string ref
(* !uses filename! *)
val init : unit -> unit

val save : unit -> unit
val save_with_help : unit -> unit
(*:
val load : unit -> unit
val append : string -> unit
*)

   
  (*** To create your own options types ... *)
  
type value =
  Module of module_
| Value of  string
| List of value list
| SmallList of value list
  
and module_ =
  (string * value) list

val define_type :
  string -> (value -> 'a) -> ('a -> value) -> 'a type_

val to_value : 'a type_ -> 'a -> value
val from_value : 'a type_ -> value -> 'a
  
 
  (* parameterized options *)
val list_option : 'a type_ -> 'a list type_
val smalllist_option : 'a type_ -> 'a list type_
val sum_option : (string * 'a) list -> 'a type_
val tuple2_option :  
  'a type_ * 'b type_ -> ('a * 'b) type_
val tuple3_option : 'a type_ * 'b type_ * 'c type_ ->
  ('a * 'b * 'c) type_
  
val value_to_string : value -> string
val value_to_int : value -> int
val value_to_bool : value -> bool
val value_to_float : value -> float
val value_to_string2 : value -> string * string
val value_to_list : (value -> 'a) -> value -> 'a list
val value_to_path : value -> string list

val string_to_value : string -> value
val int_to_value : int -> value
val bool_to_value : bool -> value
val float_to_value : float -> value
val string2_to_value : string * string -> value
val list_to_value : ('a -> value) -> 'a list -> value
val smalllist_to_value : ('a -> value) -> 'a list -> value
val path_to_value : string list -> value

val bool_of_string : string -> bool


val help : out_channel -> unit

val option_hook : 'a t -> (unit -> unit) -> unit
val type_hook : 'a type_ -> ('a t -> unit) -> unit
