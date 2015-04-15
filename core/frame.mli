(* see Efuns.frame, Efuns.status types *)

(* creation *)

val create : 
  Efuns.window -> string option -> Efuns.buffer -> Efuns.frame
(* diff? *)
val create_without_top :
  Efuns.window -> string option -> Efuns.buffer -> Efuns.frame
val create_inactive : 
  Efuns.window -> Efuns.buffer -> Efuns.frame

(* load/save/kill *)

val load_file : Efuns.window -> string -> Efuns.frame

(* ugly: you may want to use Multi_buffers.set_previous_frame just before *)
val change_buffer : Efuns.window -> string -> unit
val change_buffer_hooks: Efuns.action_name list Options.option_record

val save_buffer : Efuns.frame -> unit

val kill : Efuns.frame -> unit
val kill_all : Efuns.window -> unit
exception BufferKilled
val unkill : Efuns.window -> Efuns.frame -> unit

(* find *)

val find_buffer_frame : Efuns.buffer -> Efuns.frame

(* display *)

val display : Efuns.top_window -> Efuns.frame -> unit

(* ?? *)
val install : Efuns.window -> Efuns.frame -> unit

(* status line *)

val status_format : (Efuns.status_info * (int * int)) list ref
val status_print : Efuns.status -> string -> Efuns.status_info -> unit
val status_modified : Efuns.frame -> bool -> unit
val status_col : Efuns.frame -> int -> unit
val status_major_mode : Efuns.frame  -> unit
val status_line : Efuns.frame -> int -> unit
val status_name : Efuns.frame -> string -> unit

(* cursor *)

val set_cursor : Efuns.frame -> unit

val point_to_x_when_no_cutline : Efuns.buffer -> Text.point -> int
val cursor_to_coord : Efuns.frame -> int -> int -> Text.coord

(* x y screen coordinates *)
val move_point : Efuns.frame -> Text.point -> int -> int -> unit

(* getters/setters *)

val current_dir : Efuns.frame -> string

(* dumpers *)

(* iterator *)

val to_frame : (Efuns.buffer -> Text.point -> 'a) -> Efuns.frame -> 'a

(* misc *)

val editname : string
val active : Efuns.frame -> unit
exception FoundFrame of Efuns.frame

val bindings_help : Efuns.frame -> unit

(*
val display_line : Efuns.top_window -> Efuns.frame -> string -> int -> unit
val update_table : 'a -> Efuns.frame -> unit
val resize : Efuns.frame -> unit 
*)
