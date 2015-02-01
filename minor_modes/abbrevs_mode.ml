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

open Options
open Abbrevs
open Text
open Simple
open Efuns
open Top_window

  
let abbreviations = define_option ["abbrevs_mode"; "abbrevs"] ""
    (list_option string2_option) []
  
let abbrevs = Hashtbl.create 11
let _ =
  Utils.hash_add_assoc abbrevs !!abbreviations
  
let install buf =
  try
    ignore (get_local buf abbrev_table)
  with _ -> 
      set_local buf abbrev_table abbrevs

let mode = Ebuffer.new_minor_mode "abbrevs" [install]

let abbrevs_chars = define_option ["abbrevs_mode"; "abbrevs_chars"] ""
    string_option " "
  
let find_matching  frame = self_insert_command frame; highlight_paren frame
let char_expand_abbrev frame =
  expand_sabbrev frame; self_insert_command frame
  
let _ = 
  let chars = !!abbrevs_chars in
  for i = 0 to String.length chars - 1 do
    Keymap.add_binding mode.min_map [NormalMap, Char.code chars.[i]]
      char_expand_abbrev
  done

let _ = 
  define_action "abbrevs_mode" 
    (fun frame -> 
      let buf = frame.frm_buffer in
      if Ebuffer.modep buf mode then begin
          Ebuffer.del_minor_mode buf mode
        end else
        Ebuffer.set_minor_mode buf mode)
    
    