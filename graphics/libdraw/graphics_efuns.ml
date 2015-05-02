open Common

open Efuns

(*****************************************************************************)
(* Draw API *)
(*****************************************************************************)
open Xdraw

(* helper *)
let move_to col line =
  ()
(*
  let (w,h) = Graphics.text_size "d" in
  let size_y = Graphics.size_y () in
  Graphics.moveto (w * col) (size_y - h - (line * h))
*)
  

let clear_eol col line len =
  pr2 (spf "clear_eol: %d %d %d" col line len);
(*
  move_to col line;
  let (w,h) = Graphics.text_size "d" in
  Graphics.set_color Graphics.white;
  Graphics.fill_rect (Graphics.current_x()) (Graphics.current_y())
    (w * len) h;
*)
  ()

let draw_string col line  str  offset len   attr =
  pr2 (spf "draw_string %d %d %s %d %d %d"
         col line str offset len attr);
(*
  let (w,h) = Graphics.text_size "d" in
  move_to col line;
  Graphics.set_color Graphics.white;
  Graphics.fill_rect (Graphics.current_x()) (Graphics.current_y())
    (w * len) h;
  Graphics.set_color Graphics.black;
  move_to col line;
  Graphics.draw_string (String.sub str offset len);
*)
  ()

let update_display () =
  pr2 ("update_displays")

let backend = { 
  clear_eol = clear_eol; 
  draw_string = draw_string; 
  update_display = update_display;
  update_window_title = (fun _ -> ());
}


(*****************************************************************************)
(* paint/configure/expose *)
(*****************************************************************************)

(*****************************************************************************)
(* The main UI *)
(*****************************************************************************)

let init init_files = 

  (*-------------------------------------------------------------------*)
  (* Graphics initialisation *)
  (*-------------------------------------------------------------------*)

  Draw.initdraw None "test_draw_ml";

  (*-------------------------------------------------------------------*)
  (* Window creation *)
  (*-------------------------------------------------------------------*)

  Draw.set_color 10 10 10 255;
  Draw.line 10 10 100 100;

  (*-------------------------------------------------------------------*)
  (* Creation of core DS of Efuns (buffers, frames, top_window) *)
  (*-------------------------------------------------------------------*)

  (* loc.loc_height <- 45; *)
  (* will boostrap and use a newly created *help* buffer *)
  let top_window = Top_window.create () in
  (* the *bindings* buffer *)
  Interactive.create_bindings () |> ignore;
  (* open the first buffers *)
  init_files |> List.iter (fun name ->
    Frame.load_file top_window.window name |> ignore
  );

  (*-------------------------------------------------------------------*)
  (* Layout *)
  (*-------------------------------------------------------------------*)

    (*-------------------------------------------------------------------*)
    (* main view *)
    (*-------------------------------------------------------------------*)

  top_window.graphics <- Some backend;
  Top_window.update_display ();

  (*-------------------------------------------------------------------*)
  (* Events *)
  (*-------------------------------------------------------------------*)

  (*-------------------------------------------------------------------*)
  (* End *)
  (*-------------------------------------------------------------------*)



  Draw.string 200 200 "this is an ocaml test";

  Unix.sleep 5;
  ()


(*
let init location displayname =

  (* Main loop *)
  let rec loop () =
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
*)
