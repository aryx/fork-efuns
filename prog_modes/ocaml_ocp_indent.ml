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
(* Support for ocp-indent (https://github.com/OCamlPro/ocp-indent) in Efuns.
 *
 * To use ocp-indent from Efuns simply type 'opam install ocp-indent', which
 * should make available an 'ocp-indent' program in your PATH.
 * Then simply use TAB to indent your program or M-x indent_buffer.
 *
 * See https://github.com/OCamlPro/ocp-indent/blob/master/.ocp-indent 
 * for the different possibilities.
 *
 * Some source of inspiration for the code in this file:
 *  - ocp-indent.el in the ocp-indent source
 *
 * related:
 *  - tuareg mode indenter
 *  - default caml mode indenter
 *  - ocamlformat: but this has a different purpose. ocp-indent
 *    is just about indentation (how many spaces put at the bol)
 *    whereas ocamlformat can reformat completely expressions?
 *
 * later:
 *  - use ocamlformat for M-x indent-buffer so re-pretty-print to fit
 *    80 columns
 *)

(*****************************************************************************)
(* Constants and globals *)
(*****************************************************************************)
(* I assume this program is in your PATH *)
let external_program = "ocp-indent"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* External program communication *)
(*****************************************************************************)

(*****************************************************************************)
(* Minor mode *)
(*****************************************************************************)
(* alt: ? not a minor mode but just by setting Tab? *)

let mode = Ebuffer.new_minor_mode  "ocp-indent" []

let ocp_indent_mode = 
  Minor_modes.toggle_minor mode
[@@interactive]

(*****************************************************************************)
(* Setup *)
(*****************************************************************************)

let _ =
  Hook.add_start_hook (fun () ->
    ()
  )
