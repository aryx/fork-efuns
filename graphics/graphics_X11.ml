open Common

open Efuns



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

(*
  WX_xterm.setHighlight display 2;
  Dyneval.init true;
  Eval.load top_window "Efunsrc";
  Efuns.init location; (* launch second hooks *)
*)

  let _ = Interactive.create_bindings location in

  (* open the fisrt buffers *)
  !init_files +> List.iter (fun name ->
    let _ = Frame.load_file top_window.top_windows name in ()
  );
  !init_frames +> List.iter (fun str -> 
    let top_window = Top_window.create top_window.top_location
      (Window.display top_window) 
    in
    let _ = Frame.load_file top_window.top_windows str in ()
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
        let key = status.Graphics.key in
        pr2 (spf "key: %c" key)

    )
  in
  loop ()
