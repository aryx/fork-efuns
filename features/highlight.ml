(*s: features/highlight.ml *)
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

(*s: function [[Simple.unhightlight_region]] *)
let unhightlight_region buf debut fin =
  let text = buf.buf_text in
  Text.with_new_point text (fun curseur ->
  Text.with_new_point text (fun final ->
    Text.set_position text curseur debut;
    Text.set_position text final fin;
    while curseur < final do
      let attr = Text.get_attr text curseur in
      Text.set_attr text curseur (attr land (lnot Text.highlight_bit));
      Text.fmove text curseur 1;
    done;
    buf.buf_modified <- buf.buf_modified + 1
  ))
(*e: function [[Simple.unhightlight_region]] *)

(*s: function [[Simple.hightlight_region]] *)
let hightlight_region buf debut fin =
  let text = buf.buf_text in
  Text.with_new_point text (fun curseur ->
  Text.with_new_point text (fun final ->
    Text.set_position text curseur debut;
    Text.set_position text final fin;
    while curseur < final do
      let attr = Text.get_attr text curseur in
      Text.set_attr text curseur (attr lor Text.highlight_bit);
      Text.fmove text curseur 1
    done;
    buf.buf_modified <- buf.buf_modified + 1
  ))
(*e: function [[Simple.hightlight_region]] *)

(*s: constant [[Simple.highlighted]] *)
(* hightlighting of regions *)  
let highlighted = ref None
(*e: constant [[Simple.highlighted]] *)
(*s: constant [[Simple.highlighted_chars]] *)
let highlighted_chars = ref []
(*e: constant [[Simple.highlighted_chars]] *)

(*s: function [[Simple.unhightlight]] *)
let unhighlight _frame =
  (*s: [[Simple.unhightlight()]] handle highlighted chars *)
  !highlighted_chars |> List.iter (fun (buf,curseur,attr) ->
    let text = buf.buf_text in
    Text.set_attr text curseur attr;
    buf.buf_modified <- buf.buf_modified + 1;
    Text.remove_point text curseur
  );
  highlighted_chars := [];
  (*e: [[Simple.unhightlight()]] handle highlighted chars *)
  match !highlighted with
  | None -> ()
  | Some (frame,debut,fin) -> 
     (*   if !keypressed <> XK.xk_Pointer_Drag1 then *)
     highlighted := None;
     let (buf, text, _) = Frame.buf_text_point frame in

     Text.with_new_point text (fun curseur ->
     Text.with_new_point text (fun final ->
       Text.set_position text curseur debut;
       Text.set_position text final fin;

       let str = Text.region text curseur final in
       Copy_paste.kill_string str;
       (* ??? WX_xterm.set_cutbuffer xterm str; for interop? *)

     ));
     unhightlight_region buf debut fin
(*e: function [[Simple.unhightlight]] *)
  
(*s: function [[Simple.highlight]] *)
let highlight frame =
  let frame =
    match !highlighted with
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
    match !highlighted with
    | None -> pos1, pos2
    | Some (_frame, d, f) ->
        if pos1 > d
        then unhightlight_region buf d pos1; 
        if pos2 < f
        then unhightlight_region buf pos2 f;
        if pos1 < d 
        then pos1, d
        else
          if pos2 > f 
          then f, pos2
          else pos1, pos1
  in
  highlighted := Some (frame, pos1, pos2);
  hightlight_region buf pos_debut_to_hl pos_fin_to_hl
(*e: function [[Simple.highlight]] *)

(*s: toplevel [[Highlight]] starting hook *)
let _ =
  Hook.add_start_hook (fun () ->
    (* unhighlight region *)
    Hook.add_hook Top_window.handle_key_start_hook unhighlight;      
  )
(*e: toplevel [[Highlight]] starting hook *)
(*e: features/highlight.ml *)
