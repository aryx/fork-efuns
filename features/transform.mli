
val fill_paragraph : Efuns.action
(*
val single_char : string
val simplify : Text.t -> Text.point -> Text.point -> unit
*)

val align_char : Efuns.action

(* helpers *)

(* this maintains the \n and generate entries for empty lines *)
val lines: string -> string list
