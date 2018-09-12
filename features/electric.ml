(*s: features/electric.ml *)
(*s: copyright header2 *)
(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
(*e: copyright header2 *)
open Efuns

(*s: function [[Simple.previous_char]] *)
let previous_char frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  if Text.bmove_res text point 1 = 0 then raise Not_found;
  let c = Text.get_char text point in
  Text.fmove text point 1;
  c
(*e: function [[Simple.previous_char]] *)

(*s: function [[Simple.hungry_char]] *)
let hungry_char c = 
  c = ' ' || c = '\n' || c = '\t'
(*e: function [[Simple.hungry_char]] *)

(*s: function [[Simple.hungry_electric_delete]] *)
let hungry_electric_delete frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  text |> Text.with_session (fun () ->
   let c1 = previous_char frame in
   Edit.delete_backspace_char frame;
   let c2 = previous_char frame in
   if hungry_char c1 && hungry_char c2 then
     try
       Edit.delete_backspace_char frame;
       while 
         let c = previous_char frame in
         hungry_char c        
       do
         Edit.delete_backspace_char frame
       done;
       Edit.insert_char frame ' '
     with Not_found -> ()
  )
[@@interactive]
(*e: function [[Simple.hungry_electric_delete]] *)

(*s: function [[Simple.electric_insert_space]] *)
let electric_insert_space frame =
  Edit.self_insert_command frame;
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let line_len = Text.point_to_bol text point in
  if line_len > 75 then
    Text.with_dup_point text point (fun mark ->
      try
        while (Move.backward_word buf mark;
            Text.point_to_bol text mark > 75) do () done;
        Move.forward_word buf mark; 
        Move.backward_word buf mark;
        Text.insert text mark "\n"
      with Not_found -> ()
    )
(*e: function [[Simple.electric_insert_space]] *)
(*e: features/electric.ml *)
