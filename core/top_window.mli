(* see Efuns.top_window type *)

(* creation *)

val create : unit -> Efuns.top_window

(* display *)

(* see also Efuns.backend : Efuns.top_window -> Xdraw.graphics_backend *)
val update_display : unit -> unit

(* event handler *)

val handler : Efuns.top_window -> Xtypes.event -> unit

(* keyboard event *)

(* internal: handle_key() *)
val handle_key_start_hook : (Efuns.frame -> unit) list Local.var
val handle_key_end_hook : (unit -> unit) list Local.var

val keypressed : Efuns.keySym ref

(* mouse event *)

val mouse_set_active : Efuns.top_window -> Efuns.frame

(* menus *)

val file_menu : (string * string) list Options.option_record
val edit_menu : (string * string) list Options.option_record
val help_menu : (string * Efuns.action) array ref
val buffers_menu : (Efuns.top_window -> unit -> unit -> unit) ref

(* message *)

val message : Efuns.top_window -> string -> unit
val clear_message : Efuns.top_window -> unit

val mini_message : Efuns.frame -> string -> unit

(* cursor *)
(* used by cursor thread *)
val cursor_on: Efuns.top_window -> unit
val cursor_off: Efuns.top_window -> unit


(* misc *)
val wrap : Efuns.top_window -> (Efuns.top_window -> unit) -> unit -> unit
val check_abort : 'a -> 'b
val delete_window : 'a -> 'b

(*
val dummy_action : 'a -> unit
val try_map : Efuns.frame -> Efuns.key -> unit
val set_cursor_on : Efuns.top_window -> Efuns.frame -> unit
val set_cursor_off : Efuns.top_window -> Efuns.frame -> unit
val cursor_on : Efuns.top_window -> unit
val cursor_off : Efuns.top_window -> unit
val clean_display : unit -> unit
val resize_window : Efuns.window -> int -> int -> int -> int -> unit
val find_frame : Efuns.window -> int -> int -> Efuns.frame
val mouse_x : int ref
val mouse_y : int ref
val find_selected_frame : Efuns.top_window -> Efuns.frame
val meta : int ref
val handle_key : Efuns.top_window -> int -> Efuns.keySym -> unit
val wrap_item :
  Efuns.top_window -> 'a * (Efuns.frame -> unit) -> 'a * (unit -> unit)
val scroll_to_frame : 'a -> Efuns.top_window -> unit
val menus : (string * (string * string) list) list Options.option_record
*)
