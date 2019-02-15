(* Yoann Padioleau
 *
 * Copyright (C) 2019 Yoann Padioleau
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A basic macro system for Efuns.
 *
 * Most of the work is done in top_window.ml.
 * Top_window.handle_key_and_macro records events at the key level.
 * An alternative would be to record at the level of frame actions
 * but certain actions like self_insert_command use globals which
 * complicate things.
 *
 * less: could modify the modeline to show that we are recording
 * a macro; I often get lost.
 *)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let start_macro frame = 
  Message.message frame "Defining kbd macro...";
  Top_window.in_start_macro := true;
  ()
[@@interactive]

let end_macro frame =
  (* ugly: when recording a macro, the keys used to close the macro
   * are also recorded; When calling a macro we must not process
   * this last action
   *)
  if !Top_window.in_call_macro
  then ()
  else begin
    Top_window.in_start_macro := false;
    Message.message frame "Keyboard macro defined";
  end

[@@interactive]

let call_macro _frame =
  Top_window.in_call_macro := true
[@@interactive]
