(* see Efuns.top_window type *)
type t = Efuns.top_window

(* creation *)

val create : unit -> t

(* display *)

(* see also Efuns.backend : t -> Xdraw.graphics_backend *)
val update_display : unit -> unit

(* event handler *)

val handler : t -> Xtypes.event -> unit

(* keyboard event *)

(* internal: handle_key() *)
val handle_key_start_hook : (Efuns.frame -> unit) list Var.t
val handle_key_end_hook : (unit -> unit) list Var.t

val keypressed : Efuns.keySym ref

(* mouse event *)

val mouse_set_active : t -> Efuns.frame

(* menus *)

val file_menu : (string * string) list Options.option_record
val edit_menu : (string * string) list Options.option_record
val help_menu : (string * Efuns.action) array ref
val buffers_menu : (t -> unit -> unit -> unit) ref

(* message *)

val message2 : t -> string -> unit
val clear_message : t -> unit

val mini_message : Efuns.frame -> string -> unit

(* cursor *)
(* used by cursor thread *)
val cursor_on: t -> unit
val cursor_off: t -> unit


(* misc *)
val wrap : t -> (t -> unit) -> unit -> unit

(*
val dummy_action : 'a -> unit
val try_map : Efuns.frame -> Efuns.key -> unit
val set_cursor_on : t -> Efuns.frame -> unit
val set_cursor_off : t -> Efuns.frame -> unit
val cursor_on : t -> unit
val cursor_off : t -> unit
val clean_display : unit -> unit
val resize_window : Efuns.window -> int -> int -> int -> int -> unit
val find_frame : Efuns.window -> int -> int -> Efuns.frame
val mouse_x : int ref
val mouse_y : int ref
val find_selected_frame : t -> Efuns.frame
val meta : int ref
val handle_key : t -> int -> Efuns.keySym -> unit
val wrap_item :
  t -> 'a * (Efuns.frame -> unit) -> 'a * (unit -> unit)
val scroll_to_frame : 'a -> t -> unit
val menus : (string * (string * string) list) list Options.option_record
*)
