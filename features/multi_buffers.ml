(*s: features/multi_buffers.ml *)
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

(*s: constant Complex.up_buffer *)
let up_buffer = ref ""
(*e: constant Complex.up_buffer *)


  
(*s: function Complex.down_buffer *)
let down_buffer frame = 
  up_buffer := frame.frm_buffer.buf_name
(*e: function Complex.down_buffer *)

(*s: function Complex.up_buffer *)
let up_buffer frame =
  if !up_buffer = "" 
  then raise Not_found;
  Select.set_previous_frame frame;
  Frame.change_buffer frame.frm_window !up_buffer
(*e: function Complex.up_buffer *)
  
(*s: function Complex.left_buffer *)
let left_buffer frame =
  Select.set_previous_frame frame;
  Frame.change_buffer frame.frm_window (
    match !Select.prev_buffers with
    | name :: buffer :: tail ->
        Select.prev_buffers := tail @ [name]; 
        buffer
    | _ -> raise Not_found)
(*e: function Complex.left_buffer *)

(*s: function Complex.right_buffer *)
let right_buffer frame =
  Select.set_previous_frame frame;
  Frame.change_buffer frame.frm_window (match !Select.prev_buffers with
      name :: tail ->
        begin
          match List.rev tail with
            buffer :: tail ->
              Select.prev_buffers := name :: (List.rev tail);
              buffer
          | _ -> raise Not_found
        end
    | _ -> raise Not_found)
(*e: function Complex.right_buffer *)
  

(*e: features/multi_buffers.ml *)
