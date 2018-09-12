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

(*s: function [[Simple.buffer_list]] *)
let buffer_list frame =
  (Globals.editor()).edt_buffers |> Common.hash_to_list |> List.map fst
(*e: function [[Simple.buffer_list]] *)

(*s: constant [[Select.prev_buffers]] *)
(* todo: maybe this should be in frame.ml, next to change_buffer,
 * so change_buffer can maintain it instead of imposing to everyone
 * to use set_previous_frame just before Frame.change_buffer.
 * todo: or do via change_buffer_hook?
 *)
let prev_buffers = ref []
(*e: constant [[Select.prev_buffers]] *)
(*s: function [[Select.set_previous_frame]] *)
let set_previous_frame frame = 
  let name = frame.frm_buffer.buf_name in
  prev_buffers := name :: (Utils.list_removeq !prev_buffers name)
(*e: function [[Select.set_previous_frame]] *)
(*s: function [[Select.get_previous_frame]] *)
let get_previous_frame () = 
  match !prev_buffers with
  | [] -> ""
  | x::xs -> x
(*e: function [[Select.get_previous_frame]] *)

(* C-M-l *)
let switch_to_other_buffer frame =
  let default = get_previous_frame () in
  set_previous_frame frame;
  Frame.change_buffer frame.frm_window default
  

(*s: constant [[Select.buf_hist]] *)
let buf_hist = ref []
(*e: constant [[Select.buf_hist]] *)
(*s: function [[Select.select_buffer]] *)
let select_buffer frame request default action =
  Select.select frame (request^"(default :"^ default ^ ") ") buf_hist ""
    (fun _ -> buffer_list frame) 
    (fun s ->s) 
    (fun str ->
      let str = if str = "" then default else str in
      action str)
(*e: function [[Select.select_buffer]] *)


(*s: function [[Simple.next_buffer]] *)
(* useful when want to iterate over all buffers, e.g. in dabbrev_expand *)
let next_buffer buf =
  let buf_list = Utils.list_of_hash (Globals.editor()).edt_buffers in
  let rec iter list =
    match list with
      [] -> raise Not_found 
    | (name,b) :: tail ->
        if b == buf 
        then 
          match tail with
          | [] -> snd (List.hd buf_list) (* go back to head *)
          | (_,b)::_ -> b
        else
          iter tail
  in
  iter buf_list
(*e: function [[Simple.next_buffer]] *)

(*s: function [[Simple.kill_buffer]] *)
let kill_buffer frame =
  let window = frame.frm_window in
  let buf = frame.frm_buffer in

  let new_buf = 
    match !prev_buffers with
    | x::xs when x <> buf.buf_name -> 
        prev_buffers := xs;
        (try 
          Hashtbl.find (Globals.editor()).edt_buffers x
        with Not_found -> 
          next_buffer buf
        )
    | _ -> next_buffer buf 
  in

  let _new_frame = Frame.create window None new_buf in
  if buf.buf_shared = 0 
  then Ebuffer.kill buf
[@@interactive]
(*e: function [[Simple.kill_buffer]] *)

(*s: constant [[Complex.up_buffer]] *)
let up_buffer = ref ""
(*e: constant [[Complex.up_buffer]] *)
(*s: function [[Complex.down_buffer]] *)
let down_buffer frame = 
  up_buffer := frame.frm_buffer.buf_name
[@@interactive]
(*e: function [[Complex.down_buffer]] *)

(*s: function [[Complex.up_buffer]] *)
let up_buffer frame =
  if !up_buffer = "" 
  then raise Not_found;
  set_previous_frame frame;
  Frame.change_buffer frame.frm_window !up_buffer
[@@interactive]
(*e: function [[Complex.up_buffer]] *)
  
(*s: function [[Complex.left_buffer]] *)
let left_buffer frame =
  set_previous_frame frame;
  Frame.change_buffer frame.frm_window 
    (match !prev_buffers with
    | name :: buffer :: tail ->
        prev_buffers := tail @ [name]; 
        buffer
    | _ -> raise Not_found
    )
(*e: function [[Complex.left_buffer]] *)

(*s: function [[Complex.right_buffer]] *)
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
(*e: function [[Complex.right_buffer]] *)

(*s: function [[Complex.change_buffer]] *)
let change_buffer frame =
  let default = get_previous_frame () in
  set_previous_frame frame;
  select_buffer frame " Switch to buffer: " default (fun str ->
    Frame.change_buffer frame.frm_window str
  )
[@@interactive]
(*e: function [[Complex.change_buffer]] *)

(*e: features/multi_buffers.ml *)
