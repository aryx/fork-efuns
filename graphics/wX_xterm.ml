open Common

let clear_eol col line len =
  pr2 (spf "WX_xterm.clear_eol: %d %d %d" col line len)

let draw_string col line str offset len attr =
    pr2 (spf "WX_xterm.draw_string %d %d %s %d %d %d"
           col line str offset len attr)
