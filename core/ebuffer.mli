
(* see Efuns.buffer, Efuns.major_mode, Efuns.minor_mode types *)
type t = Efuns.buffer

(* creation *)

val create : string -> string option -> Text.t -> Efuns.map -> t
val create_buf_hook : (t -> unit) list Store.var

(* load/save/kill *)

val read : string -> Efuns.map -> t
val save : t -> unit
val save_buffer_hooks: Efuns.action_name list Options.option_record
val saved_buffer_hooks : (t -> unit) list Store.var

(* This should not be used in plugins and hoping it will kill a buffer.
 * What you want is kill the frame managing the buffer, and then
 * once you're sure there are no more reference to the buf
 * then you can kill it.
 *)
val kill : t -> unit

(* find *)

val find_buffer_opt: string -> t option
(* create or find existing one *)
val default : string -> t

(* getters/setters *)

exception BufferAlreadyOpened
val change_name : t -> string -> unit

val set_mark : t -> Text.point -> unit
val get_mark : t -> Text.point -> Text.point
val remove_mark : t -> unit

val create_syntax_table : unit -> bool array
val default_syntax_table : bool array

val tab_size : int ref

(* major modes *)

val new_major_mode : string -> (t -> unit) option -> Efuns.major_mode
val set_major_mode : t -> Efuns.major_mode -> unit
val fondamental_mode : Efuns.major_mode

val modes_alist : (string * Efuns.major_mode) list Store.var
(* use modes_alist to try to automatically set the major mode *)
val set_buffer_mode : t -> unit

(* minor modes *)

val new_minor_mode : string ->  (t -> unit) list -> Efuns.minor_mode
val set_minor_mode : t -> Efuns.minor_mode -> unit
val del_minor_mode : t -> Efuns.minor_mode -> unit
val modep : t -> Efuns.minor_mode -> bool

(* display *)
val compute_representation : t -> int -> Text.line

(* input *)
val get_binding : t -> Efuns.key list -> Efuns.binding

(* misc *)
val message : t -> string -> unit

(*
val catch : 
  (string -> string, unit, string) format ->
  t -> (unit -> unit) -> unit
val regexp_alist : (Str.regexp * Efuns.major_mode) list ref
val suffix_reg : Str.regexp
val get_name : string -> string
*)
