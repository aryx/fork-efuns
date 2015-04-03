(*s: features/structure.ml *)
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

(*s: function Simple.next_hole *)
(* a hole is two consecutive '^' chars *)
let next_hole frame = 
  let buf = frame.frm_buffer in
  let point = frame.frm_point in
  let text = buf.buf_text in
  let curseur = Text.dup_point text point in
  while 
    not ((Text.get_char text curseur = '^') && (Text.fmove_res text curseur 1 = 1) &&
      (Text.get_char text curseur = '^')) && (Text.fmove_res text curseur 1 = 1) do () done;
  if Text.get_char text curseur = '^' then
    (Text.bmove text curseur 1;
      Text.delete text curseur 2;
      Text.goto_point text point curseur);
  Text.remove_point text curseur
(*e: function Simple.next_hole *)


(*s: function Simple.insert_structure *)
let insert_structure s frame =
  let buf = frame.frm_buffer in
  let point = frame.frm_point in
  let text = buf.buf_text in
  Text.insert text point s;
  next_hole frame
(*e: function Simple.insert_structure *)

(*s: function Simple.install_structures *)
let install_structures buf list =
  list |> List.iter (fun (key, s) ->
    Keymap.add_binding buf.buf_map key (insert_structure s)
  )
(*e: function Simple.install_structures *)

(*e: features/structure.ml *)
