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
(*open Common*)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Support for Merlin under Efuns!
 * (I actually had troubles setting up Merlin for my Emacs 23 ...)
 *
 * Look at merlin/doc/dev/PROTOCOL.md to see the list of commands.
 * You can also experiment with merlin directly from the command line:
 * 
 *   $ ocamlmerlin single errors -filename foo.ml < foo.ml
 * 
 * Note the -filename option is optional but very important for multi-file
 * support. Indeed, it is this option that allows merlin to process
 * the .merlin in your project.
 *
 * related: 
 * - ocamlspotter
 * - otags
 * - pfff (lang_ml/ and lang_cmt/)
 *)


let mode = Ebuffer.new_minor_mode  "Merlin" [(fun buf ->
  ()
)]

let _ = 
  Action.define_action "merlin_mode" (Minor_modes.toggle_minor mode)
