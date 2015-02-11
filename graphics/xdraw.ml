

type graphics_backend = {
  clear_eol: int -> int -> int -> unit;
  draw_string: int -> int -> string -> int -> int -> int -> unit;
  update_displays: unit -> unit;
}
