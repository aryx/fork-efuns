(* Yoann Padioleau
 *
 * Copyright (C) 2018 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Efuns

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * todo:
 *  - C-x r y
 * less:
 *  - maintain point (deleting whole thing simplify things but complicates
 *    other)
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type rectangle_line = {
  before: string;
  rect_part: string;
  (* this usually contains the \n *)
  after: string;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let string_of_rectangle_line rline = 
  rline.before ^ rline.rect_part ^ rline.after

let apply_to_region f region (cola, colb) =
  let xs = Transform.lines region in
  (* sometimes rectangle operations are applied on big regions
   * containing empty lines where String.sub would fail; we do
   * not want though to abort the whole operation hence the
   * try below
   *)
  let rlines = xs |> List.map (fun s ->
    { before    = 
       (* less: maybe should insert as many space as cola here, because
        * when you insert a rectangle on an empty line at a certain column,
        * you want extra space to match before that column
        *)
       (try String.sub s 0 cola with Invalid_argument _ -> "");
      rect_part = 
       (try String.sub s cola (colb - cola) with Invalid_argument _ -> "");
      after = 
       (try String.sub s colb (String.length s - colb) 
       (* we need to maintain the \n *)
       with Invalid_argument _ -> "\n");
    }
  ) in
 rlines |> List.map f |> List.map string_of_rectangle_line |> String.concat ""
  


let transform_marked_rectangle frame f =
  let (buf, text, point) = Frame.buf_text_point frame in
  let mark =
    match buf.buf_mark with
      | None -> failwith "No mark set"
      | Some mark -> 
        buf.buf_mark <- None;
        mark
  in
  let (a,b) = if mark > point then (point,mark) else (mark,point) in

  let cola = Text.point_col text a in
  let colb = Text.point_col text b in
  let cola, colb = if colb > cola then (cola, colb) else (colb, cola) in
  Text.with_dup_point text a (fun a ->
  Text.with_dup_point text b (fun b ->
    (* enlarge the rectangle to the whole lines *)
    Text.bmove text a (Text.point_to_bol text a);
    Text.fmove text b (Text.point_to_eol text b);

    let delta = Text.distance text a b in
    let region = Text.sub text a delta in

    let region' = apply_to_region f region (cola, colb) in

    text |> Text.with_session (fun () ->
      Text.delete text a delta;
      Text.insert text a region';
      Text.remove_point text mark;
    );

    Text.remove_point text mark    
  ))  

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let rectangle_insert frame =
  Select.simple_select frame "String rectangle: " (fun s ->
   transform_marked_rectangle frame (fun rline ->
     { rline with rect_part = s }
  ))
[@@interactive]

let rectangle_kill frame =
  transform_marked_rectangle frame (fun rline ->
     { rline with rect_part = "" }
  )
[@@interactive]
