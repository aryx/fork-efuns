open Common


(* helper *)
let move_to col line =
  let (w,h) = Graphics.text_size "d" in
  let size_y = Graphics.size_y () in
  Graphics.moveto (w * col) (size_y - h - (line * h))
  

let clear_eol () col line len =
  pr2 (spf "WX_xterm.clear_eol: %d %d %d" col line len);
  move_to col line;
  let (w,h) = Graphics.text_size "d" in
  Graphics.set_color Graphics.white;
  Graphics.fill_rect (Graphics.current_x()) (Graphics.current_y())
    (w * len) h;
  ()

let draw_string () col line  str  offset len   attr =
  pr2 (spf "WX_xterm.draw_string %d %d %s %d %d %d"
         col line str offset len attr);
  let (w,h) = Graphics.text_size "d" in
  move_to col line;
  Graphics.set_color Graphics.white;
  Graphics.fill_rect (Graphics.current_x()) (Graphics.current_y())
    (w * len) h;
  Graphics.set_color Graphics.black;
  move_to col line;
  Graphics.draw_string (String.sub str offset len);
  ()

let update_displays () =
  pr2 ("WX_xterm.update_displays")
