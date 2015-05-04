
(* pretty limited needs *)

type graphics_backend = {
  (* drawing text boxes *)
  clear_eol: int -> int -> int -> unit;
  draw_string: int -> int -> string -> int -> int -> int -> unit;

  (* flush the display *)
  update_display: unit -> unit;

  (* windowing system interaction *)
  update_window_title: string -> unit;
}
