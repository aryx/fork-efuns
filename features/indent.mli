
(* to be set by each major programming mode for the index_xxx below to work *)
val indent_func: (Efuns.buffer -> Text.point -> Text.point -> unit) Var.t

val indent_region: Efuns.action
val indent_phrase: Efuns.action
val indent_buffer: Efuns.action


(* helpers to build an indent_func *)

(* ??? *)
type indentations = (int * (Text.position list (* eols *))) list

type 'tok indentation_stack = ('tok * int) list

val pop_to_top: 'tok indentation_stack -> 'tok indentation_stack * int
val pop_to: 'tok -> 'tok indentation_stack -> 'tok indentation_stack * int
val pop_to_kwds: 'tok -> 
  ('tok list -> 'tok indentation_stack -> 
  'tok indentation_stack * ('tok * int))

val add: int -> Text.position list -> indentations -> indentations

(* helpers to generate the indent_func above *)
val indent_between_points: 
  (Efuns.buffer -> Text.point -> Text.point -> indentations) (* get_indent*) ->
  Str.regexp (* phrase_start *) -> 
  Efuns.buffer -> Text.point -> Text.point -> unit (* indent_func *)


(* helper to generate indent_current_line to bind to TAB *)
val indent_current_line:
  (Efuns.buffer -> Text.point -> Text.point -> indentations) (* get_indent*) ->
  Str.regexp (* phrase_start *) ->
  (Efuns.buffer -> Text.point -> Text.point -> unit) (* color_region *) ->
  Efuns.action


(* internals
val set_indent : Text.t -> Text.point -> int -> unit
val pop_indentations: indentations -> int * Text.position * indentations

(* debugging *)
val print_indentations: indentations -> unit
val print_stack: ('tok * string) list -> 'tok indentation_stack -> unit
*) 
