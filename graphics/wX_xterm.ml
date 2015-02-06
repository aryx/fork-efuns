open Common


let move_to col line =
  let (w,h) = Graphics.text_size "d" in
  let size_y = Graphics.size_y () in
  Graphics.moveto (w * col) (size_y - (line * h))
  

let clear_eol () col line len =
  pr2 (spf "WX_xterm.clear_eol: %d %d %d" col line len);
  ()

let draw_string () col line  str  offset len   attr =
    pr2 (spf "WX_xterm.draw_string %d %d %s %d %d %d"
           col line str offset len attr);
  move_to col line;
  Graphics.draw_string (String.sub str offset len);
  ()

let update_displays () =
  pr2 ("WX_xterm.update_displays")
