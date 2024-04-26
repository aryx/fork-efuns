(***********************************************************************)
(*                                                                     *)
(*                               Efuns                                 *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* This is a small addons for the Str module *)

(*external replacement_text: string -> string -> string = "str_replacement_text"*)
let replace_matched _repl _matched =
  failwith "Str2.replace_matched:TODO"
(*  replacement_text repl matched*)
  

let regexp_from_list list =
  Str.regexp  
    (match list with
      [] -> ""
    | hd :: tail ->
        List.fold_left (fun str ele ->
          str ^ "\\|\\(" ^ ele ^ "\\)") ("\\(" ^ hd ^ "\\)") tail)
