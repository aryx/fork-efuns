(*s: features/indent.ml *)
(*s: copyright header efuns *)
(***********************************************************************)
(*                                                                     *)
(*                             Efuns                                   *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
(*e: copyright header efuns *)

(*s: function [[Simple.set_indent]] *)
(* modify the indentation of (point) line. Does not modify point *)
let set_indent text point offset = 
  Text.with_dup_point text point (fun curseur ->
    Text.bmove text curseur (Text.point_to_bol text curseur);
    let rec iter offset =
      let c = Text.get_char text curseur in
      if offset > 0 then
        if c = ' ' then
          (Text.fmove text curseur 1; iter (offset - 1))
        else
        if c = '\t' then
          (Text.delete text curseur 1;
           iter offset)
        else
          (Text.insert text curseur (String.make offset ' '))
      else
      if c = ' ' || c='\t' then
        (Text.delete text curseur 1;
          iter 0)
    in
    iter offset
  )
(*e: function [[Simple.set_indent]] *)

(*e: features/indent.ml *)
