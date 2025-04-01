(*s: features/mouse.ml *)
(*s: copyright header efuns *)
(***********************************************************************)
(*                                                                     *)
(*                             Efuns                                   *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
(*e: copyright header efuns *)
open Efuns
module H = Highlight

(*s: function [[Simple.highlight]] *)
let highlight frame =
  let frame =
    match !H.highlighted with
    | None -> frame
    | Some (frame, _d, _f) -> frame
  in    
  let (buf, text, point) = Frame.buf_text_point frame in
  let mark = Ebuffer.get_mark buf point in
  let pt_debut, pt_fin =
    if point < mark 
    then point, mark
    else mark, point
  in
  let pos1 = Text.get_position text pt_debut in
  let pos2 = Text.get_position text pt_fin in
  let pos_debut_to_hl, pos_fin_to_hl =
    match !H.highlighted with
    | None -> pos1, pos2
    | Some (_frame, d, f) ->
        if pos1 > d
        then H.unhighlight_region buf d pos1; 
        if pos2 < f
        then H.unhighlight_region buf pos2 f;
        if pos1 < d 
        then pos1, d
        else
          if pos2 > f 
          then f, pos2
          else pos1, pos1
  in
  H.highlighted := Some (frame, pos1, pos2);
  H.highlight_region buf pos_debut_to_hl pos_fin_to_hl
(*e: function [[Simple.highlight]] *)

let unhighlight_hook (text, debut, fin) =
  Text.with_new_point text (fun curseur ->
  Text.with_new_point text (fun final ->
    Text.set_position text curseur debut;
    Text.set_position text final fin;
    let str = Text.region text curseur final in
    Copy_paste.kill_string str;
    (* TODO WX_xterm.set_cutbuffer xterm str; for interop?  *)
  ))


(*s: function [[Simple.mouse_drag_region]] *)
(* C'est tout simple. On arrive dans cette fonction quand on est en train
de bouger la souris avec le bouton appuyer. La frame courante est donc 
correcte. On peut utiliser la position de la souris pour trouver la 
nouvelle position du curseur dans la frame. Si on en sort, on peut
ou prendre la derniere position, ou la premiere.
*)
let mouse_drag_region _frame =
  failwith "Simple.mouse_drag_region: TODO"
[@@interactive]
(*
  let top_window = Window.top frame.frm_window in
  let point = frame.frm_point in
  begin
    try
      move_point frame point !mouse_x !mouse_y
    with
      Not_found ->
        let (buf, text, _) = Frame.buf_text_point frame in
        let y = !mouse_y - frame.frm_ypos in
        if y < 0 then
          ( scroll_line frame (y-1);
            goto_point text point frame.frm_start;
            bmove text point 1; ())
        else
        if y >= frame.frm_height - 1 then
          (
            scroll_line frame (y - frame.frm_height + 2);
            goto_point text point frame.frm_end;
            fmove text point 1; ())
  end;
  highlight frame;
  let top_window = Window.top frame.frm_window in
  let xterm = top_window.top_term in
  Selection.setSelection xterm#display xterm#window XA.xa_primary
    (fun target ->
      if target = XA.xa_string then 
        match !highlighted with
          None -> raise Not_found
        | Some (frame,debut,fin) -> 
            let (buf, text, _) = Frame.buf_text_point frame in
            let curseur = new_point text in
            let final = new_point text in
            set_position text curseur debut;
            set_position text final fin;
            let str = Text.region text curseur final in
            remove_point text curseur;
            remove_point text final;
            
            1, str
      else raise Not_found
  ) !Eloop.event_time
*)
(*e: function [[Simple.mouse_drag_region]] *)

(*s: function [[Simple.mouse_yank_at_click]] *)
let  mouse_yank_at_click _frame =
failwith "Simple.mouse_yank_at_click: TODO"
[@@interactive "insert_at_point"]
(*
  let top_window = Window.top frame.frm_window in
  let frame = mouse_set_active top_window in
  let (buf, text, point) = Frame.buf_text_point frame in
  let xterm = Window.xterm top_window in
  let str = WX_xterm.get_cutbuffer xterm in
  Text.insert text point str;
  Text.fmove text point (String.length str)
*)
(*e: function [[Simple.mouse_yank_at_click]] *)

(*s: function [[Simple.mouse_save_then_kill]] *)
let mouse_save_then_kill _frame =
  failwith "Simple.mouse_save_then_kill: TODO"
[@@interactive]
(*
  let top_window = Window.top frame.frm_window in
  let frame = Top_window.find_selected_frame top_window in
  let (buf, text, point) = Frame.buf_text_point frame in
  let mark = Ebuffer.get_mark buf point in
  Text.with_new_point text (fun new_point ->
    Frame.move_point frame new_point !mouse_x !mouse_y;
    if point = new_point then begin
      let (start,term) =
        if point < mark then (point,mark) else (mark,point) 
      in
      Text.delete text start (Text.distance text start term)
    end else begin
      let xterm = Window.xterm top_window in
      goto_point text mark point;
      goto_point text point new_point;
      let str = Text.region text mark point in
      kill_string str;
      WX_xterm.set_cutbuffer xterm str;
      highlight frame
    end
  )
*)
(*e: function [[Simple.mouse_save_then_kill]] *)

(*s: function [[Simple.mouse_set_frame]] *)
let mouse_set_frame frame =
  let top_window = Window.top frame.frm_window in
  let frame = Top_window.mouse_set_active top_window in
  let (buf, text, point) = Frame.buf_text_point frame in
  let mark = Ebuffer.get_mark buf point in
  Text.goto_point text mark point;
  ()
[@@interactive "set_active_frame"]
(*e: function [[Simple.mouse_set_frame]] *)

(*s: toplevel [[Mouse]] starting hook *)
let _ =
  Hooks.add_start_hook (fun () ->
    Keymap.add_global_key [NormalMap, XK.xk_Pointer_Drag1] mouse_drag_region;
    Hooks.add_hook Highlight.unhighlight_hook unhighlight_hook;
  )
(*e: toplevel [[Mouse]] starting hook *)

(*e: features/mouse.ml *)
