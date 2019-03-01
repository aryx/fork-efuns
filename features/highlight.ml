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

(*s: function [[Simple.unhighlight_region]] *)
let unhighlight_region buf debut fin =
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
(*e: function [[Simple.unhighlight_region]] *)

(*s: function [[Simple.highlight_region]] *)
let highlight_region buf debut fin =
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
(*e: function [[Simple.highlight_region]] *)

(*s: constant [[Simple.highlighted]] *)
(* hightlighting of a region *)  
let highlighted = ref None
(*e: constant [[Simple.highlighted]] *)
(*s: constant [[Simple.highlighted_chars]] *)
let highlighted_chars = ref []
(*e: constant [[Simple.highlighted_chars]] *)
let unhighlight_hook = Store.create_abstr "unhighlight_hook"

(*s: function [[Simple.unhighlight]] *)
let unhighlight _frame =
  (*s: [[Simple.unhighlight()]] handle highlighted chars *)
  !highlighted_chars |> List.iter (fun (buf,curseur,old_attr) ->
    let text = buf.buf_text in
    (* less: could reset highlight bit instead of having to store old_attr *)
    Text.set_attr text curseur old_attr;
    buf.buf_modified <- buf.buf_modified + 1;
    Text.remove_point text curseur
  );
  highlighted_chars := [];
  (*e: [[Simple.unhighlight()]] handle highlighted chars *)
  match !highlighted with
  | None -> ()
  | Some (frame,debut,fin) -> 
     (*   if !keypressed <> XK.xk_Pointer_Drag1 then *)
     highlighted := None;
     let (buf, text, _) = Frame.buf_text_point frame in
     Hook.exec_hooks (Var.get_global unhighlight_hook) (text, debut, fin);
     unhighlight_region buf debut fin
(*e: function [[Simple.unhighlight]] *)
  
(*s: toplevel [[Highlight]] starting hook *)
let _ =
  Hook.add_start_hook (fun () ->
    Hook.add_hook Top_window.handle_key_start_hook unhighlight;      
  )
(*e: toplevel [[Highlight]] starting hook *)
(*e: features/highlight.ml *)
