
(* to be set by each major programming mode *)
val indent_func: (Efuns.buffer -> Text.point -> Text.point -> unit) Var.t

val indent_region: Efuns.action
val indent_phrase: Efuns.action
val indent_buffer: Efuns.action

(* ??? *)
val set_indent : Text.t -> Text.point -> int -> unit

(* helpers to be used by indention functions *)

(* ??? *)
type indentations = (int * (Text.position list (* eols *))) list

type 'tok indentation_stack = ('tok * int) list

val pop_to_top: 'tok indentation_stack -> 'tok indentation_stack * int
val pop_to: 'tok -> 'tok indentation_stack -> 'tok indentation_stack * int
val pop_to_kwds: 'tok -> 
  ('tok list -> 'tok indentation_stack -> 'tok indentation_stack * 'tok * int)

val pop_indentations: indentations -> int * Text.position * indentations
(* ??? *) 
val fix: int -> Text.position list -> indentations -> indentations

(* debugging *)
val print_indentations: indentations -> unit

val print_stack: ('tok * string) list -> 'tok indentation_stack -> unit
