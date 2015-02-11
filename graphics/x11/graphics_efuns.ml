open Common

open Efuns

(*****************************************************************************)
(* Draw API *)
(*****************************************************************************)

(* helper *)
let move_to col line =
  let (w,h) = Graphics.text_size "d" in
  let size_y = Graphics.size_y () in
  Graphics.moveto (w * col) (size_y - h - (line * h))
  
let clear_eol col line len =
  pr2 (spf "WX_xterm.clear_eol: %d %d %d" col line len);
  move_to col line;
  let (w,h) = Graphics.text_size "d" in
  Graphics.set_color Graphics.white;
  Graphics.fill_rect (Graphics.current_x()) (Graphics.current_y())
    (w * len) h;
  ()

let draw_string col line  str  offset len   attr =
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

let backend = { Xdraw. clear_eol; draw_string; update_displays }


(*****************************************************************************)
(* The main UI *)
(*****************************************************************************)

let init location displayname =
  Graphics.open_graph (spf " ");
  Graphics.set_window_title displayname;

  (* compute font_size and adjust size of window, or reverse
   * by setting size of font depending on size of window ?
   *)
(*
  let (h, w) = Graphics.textsize "aqd" in
  h := 
*)

  let display = "" in
  let top_window = Top_window.create location display in
  top_window.graphic <- Some backend;

(*
  WX_xterm.setHighlight display 2;
  Dyneval.init true;
  Eval.load top_window "Efunsrc";
  Efuns.init location; (* launch second hooks *)
*)

  let _ = Interactive.create_bindings location in

  (* open the first buffers *)
  !init_files +> List.iter (fun name ->
    let _ = Frame.load_file top_window.window name in ()
  );
  !init_frames +> List.iter (fun str -> 
    let top_window = Top_window.create top_window.top_location
      (*(Window.display top_window) *) "TODO_display"
    in
    let _ = Frame.load_file top_window.window str in ()
  );

  Top_window.update_display location;

(*  
  if not (Sys.file_exists (Filename.concat Utils.homedir ".efunsrc")) then
    begin
      Printf.printf "Saving .efunsrc after install"; print_newline ();
      Options.save ();
    end;
*)

(*  if !check then exit 0;   *)

  (* Main loop *)
  let rec loop () =
(*
    try
      WX_types.loop ()
    with
      SigInt -> loop ()
*)
    Graphics.loop_at_exit [
      Graphics.Button_down;
      Graphics.Key_pressed;
    ] (fun status ->
      if status.Graphics.keypressed
      then 
        let charkey = status.Graphics.key in
        let code = Char.code charkey in
        pr2 (spf "key: %c, %d" charkey code);
        let modifiers, code = 
          match code with
          | 8 | 9 | 13  -> 0, code
          | _ when code >= 1 && code <= 26 -> 
            Xtypes.controlMask, code - 1 + Char.code 'a'
          | _ -> 0, code
        in
        let evt = Xtypes.XTKeyPress (modifiers, spf "%c" charkey, code) in
        Top_window.handler top_window () evt
    )
  in
  loop ()
