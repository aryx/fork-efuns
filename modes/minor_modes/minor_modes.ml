(* Yoann Padioleau
 *
 * Copyright (C) 2015 Yoann Padioleau
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
(* Helper functions for minor modes *)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)
let toggle_minor_on_buf mode = fun buf -> 
  if Ebuffer.has_minor_mode buf mode
  then Ebuffer.del_minor_mode buf mode
  else Ebuffer.set_minor_mode buf mode

let toggle_minor mode = fun frame -> 
  let buf = frame.frm_buffer in
  toggle_minor_on_buf mode buf
