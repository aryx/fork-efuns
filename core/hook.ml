(*s: core/hook.ml *)
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

(*s: constant Efuns.start_hooks *)
(* Les hooks de lancement apres le chargement d'un module *)
let start_hooks = (ref []: (unit -> unit) list ref)
(*e: constant Efuns.start_hooks *)
(*s: function Efuns.add_start_hook *)
let add_start_hook hook = 
  start_hooks := hook :: !start_hooks
(*e: function Efuns.add_start_hook *)

(*s: function Efuns.exec_hooks *)
let exec_hooks hooks arg =
  hooks |> List.iter (fun f ->
    try f arg 
    with exn -> Globals.error "exn in hook: %s" (Common.exn_to_s exn)
  )
(*e: function Efuns.exec_hooks *)

(*s: function Efuns.add_hook *)
let add_hook hook_var hook =
  let tail = try Var.get_global hook_var with _ -> [] in
  Var.set_global hook_var (hook :: tail)
(*e: function Efuns.add_hook *)

(*e: core/hook.ml *)