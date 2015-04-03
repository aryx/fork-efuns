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

(*s: function Simple.buffer_list *)
let buffer_list frame =
  (Globals.location()).loc_buffers |> Common.hash_to_list |> List.map fst
(*e: function Simple.buffer_list *)


(*s: constant Select.prev_buffers *)
let prev_buffers = ref []
(*e: constant Select.prev_buffers *)
(*s: constant Select.next_default *)
let next_default = ref ""
(*e: constant Select.next_default *)
(*s: function Select.set_previous_frame *)
let set_previous_frame frame = 
  let name = frame.frm_buffer.buf_name in
  next_default := name;
  prev_buffers := name :: (Utils.list_removeq !prev_buffers name)
(*e: function Select.set_previous_frame *)
(*s: function Select.get_previous_frame *)
let get_previous_frame () = !next_default
(*e: function Select.get_previous_frame *)

(*s: constant Select.buf_hist *)
let buf_hist = ref []
(*e: constant Select.buf_hist *)
(*s: function Select.select_buffer *)
let select_buffer frame request default action =
  Select.select frame (request^"(default :"^ default ^ ") ") buf_hist ""
    (fun _ -> buffer_list frame) (fun s ->s) 
  (fun str ->
      let str = 
        if str = "" then default else str in
      action str)
(*e: function Select.select_buffer *)


(*s: function Simple.next_buffer *)
let next_buffer buf =
  let buf_list = Utils.list_of_hash (Globals.location()).loc_buffers in
  let rec iter list =
    match list with
      [] -> raise Not_found 
    | (name,b) :: tail ->
        if b == buf then 
          match tail with
            [] -> snd (List.hd buf_list)
          | (_,b)::_ -> b
        else
          iter tail
  in
  iter buf_list
(*e: function Simple.next_buffer *)

(*s: function Simple.kill_buffer *)
let kill_buffer frame =
  let window = frame.frm_window in
  let buf = frame.frm_buffer in
  let new_buf = next_buffer buf in
  let _new_frame = Frame.create window None new_buf in
  if buf.buf_shared = 0 
  then Ebuffer.kill buf
(*e: function Simple.kill_buffer *)

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
  set_previous_frame frame;
  Frame.change_buffer frame.frm_window !up_buffer
(*e: function Complex.up_buffer *)
  
(*s: function Complex.left_buffer *)
let left_buffer frame =
  set_previous_frame frame;
  Frame.change_buffer frame.frm_window 
    (match !prev_buffers with
    | name :: buffer :: tail ->
        prev_buffers := tail @ [name]; 
        buffer
    | _ -> raise Not_found
    )
(*e: function Complex.left_buffer *)

(*s: function Complex.right_buffer *)
let right_buffer frame =
  set_previous_frame frame;
  Frame.change_buffer frame.frm_window 
    (match !prev_buffers with
    | name :: tail ->
        (match List.rev tail with
        | buffer :: tail ->
            prev_buffers := name :: (List.rev tail);
            buffer
        | _ -> raise Not_found
        )
    | _ -> raise Not_found
    )
(*e: function Complex.right_buffer *)

let switch_to_other_buffer frame =
  let default = get_previous_frame () in
  set_previous_frame frame;
  Frame.change_buffer frame.frm_window default
  

(*e: features/multi_buffers.ml *)
