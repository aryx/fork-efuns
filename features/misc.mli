(* insertion *)

val electric_insert_space : Efuns.action

val insert_special_char : Efuns.action

(* deletion *)

val previous_char : Efuns.frame -> char

val hungry_char : char -> bool
val hungry_electric_delete : Efuns.action

(* paragraph *)

val fill_paragraph : Efuns.action
val line_comment : string Store.var

val point_at_mark : Efuns.action

(* attributes *)

val unset_attr : Efuns.action

val color : Efuns.buffer -> Str.regexp -> bool -> Text.attribute -> unit

(* misc *)

val toggle_overwrite_mode: Efuns.action

val binding_option :
  ((Efuns.mod_ident * int) list * string) Options.option_class

(*
val single_char : string
val simplify : Text.t -> Text.point -> Text.point -> unit
*)
