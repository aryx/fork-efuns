open Common

open Efuns

let init location displayname =
  let _locale = GtkMain.Main.init () in
  let win = GWindow.window
    ~title:"Efuns"
    ~width:600 ~height:600
    ~allow_shrink:true ~allow_grow:true
    ()
  in

  let quit () = 
    (*Controller.before_quit_all model;*)
    GMain.Main.quit ();
  in

  win#event#connect#delete    ~callback:(fun _  -> quit(); true) +> ignore;
  win#connect#destroy         ~callback:(fun () -> quit(); ) +> ignore;
  win#show ();

  GtkThread.main ();

(*
  Graphics.open_graph (spf " ");
  Graphics.set_window_title displayname;
*)

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

  (* open the first buffers *)
  !init_files +> List.iter (fun name ->
    let _ = Frame.load_file top_window.window name in ()
  );
  !init_frames +> List.iter (fun str -> 
    let top_window = Top_window.create top_window.top_location
      (Window.display top_window) 
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
    ()
(*
    try
      WX_types.loop ()
    with
      SigInt -> loop ()
*)
(*
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
*)
  in
  loop ()
