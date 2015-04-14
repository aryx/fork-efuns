
val is_paren_end : char -> bool
val is_paren_begin : char -> bool

val htmlp : bool ref

val mode: Efuns.minor_mode

(* used by abbrevs_mode too *)
val highlight_paren : Efuns.frame -> unit (* not an action *)

