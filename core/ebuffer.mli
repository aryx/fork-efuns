
(* creation *)

val create : 
  string -> string option -> Text.t -> Efuns.map -> 
  Efuns.buffer
val create_buf_hook : (Efuns.buffer -> unit) list Local.var

(* create or switch to existing one *)
val default : string -> Efuns.buffer

(* load/save/kill *)

val kill : Efuns.buffer -> unit
val save : Efuns.buffer -> unit

(* getters/setters *)

val get_name : string -> string
exception BufferAlreadyOpened
val change_name : Efuns.buffer -> string -> unit

val set_mark : Efuns.buffer -> Text.point -> unit
val get_mark : Efuns.buffer -> Text.point -> Text.point
val remove_mark : Efuns.buffer -> unit

val create_syntax_table : unit -> bool array
val default_syntax_table : bool array

val tab_size : int ref

(* major modes *)

val new_major_mode : string -> (Efuns.buffer -> unit) list ->  Efuns.major_mode
val set_major_mode : Efuns.buffer -> Efuns.major_mode -> unit
val fondamental_mode : Efuns.major_mode

val modes_alist : (string * Efuns.major_mode) list Local.var
(* use modes_alist to try to automatically set the major mode *)
val set_buffer_mode : Efuns.buffer -> unit

(* minor modes *)

val new_minor_mode : string ->  (Efuns.buffer -> unit) list -> Efuns.minor_mode
val set_minor_mode : Efuns.buffer -> Efuns.minor_mode -> unit
val del_minor_mode : Efuns.buffer -> Efuns.minor_mode -> unit
val modep : Efuns.buffer -> Efuns.minor_mode -> bool

(* display *)
val compute_representation : Efuns.buffer -> int -> Text.line

(* misc *)

val get_binding : Efuns.buffer -> Efuns.key list -> Efuns.binding
val message : Efuns.buffer -> string -> unit
exception Found of Efuns.buffer
val read : string -> Efuns.map -> Efuns.buffer


(*
val catch : 
  (string -> string, unit, string) format ->
  Efuns.buffer -> (unit -> unit) -> unit
val regexp_alist : (Str.regexp * Efuns.major_mode) list ref
val suffix_reg : Str.regexp
*)
