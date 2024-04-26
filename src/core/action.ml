(*s: core/action.ml *)
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
open Common
open Efuns

(*s: type [[Action.t]] *)
type t = Efuns.action
(*e: type [[Action.t]] *)

type name = Efuns.action_name

(*s: global [[Efuns.actions]] *)
let (actions : (action_name, generic_action) Hashtbl.t) = 
  Hashtbl.create 63
(*e: global [[Efuns.actions]] *)

(*s: function [[Efuns.define_action]] *)
let define_action action_name action_fun =
  (*s: sanity check action defined twice *)
  (try 
      Hashtbl.find actions action_name |> ignore;
      Error.error (spf "action \"%s\" defined twice" action_name);
   with _ -> ()
  );
  (*e: sanity check action defined twice *)
  Hashtbl.add actions action_name (FrameAction action_fun)
(*e: function [[Efuns.define_action]] *)

(*s: function [[Efuns.define_buffer_action]] *)
let define_buffer_action action_name action_fun =
  (*s: sanity check action defined twice *)
  (try 
      Hashtbl.find actions action_name |> ignore;
      Error.error (spf "action \"%s\" defined twice" action_name);
   with _ -> ()
  );
  (*e: sanity check action defined twice *)
  Hashtbl.add actions action_name (BufferAction action_fun)
(*e: function [[Efuns.define_buffer_action]] *)

(*s: function [[Efuns.get_action]] *)
let get_action action_name =
  try Hashtbl.find actions action_name
  with Not_found ->
    Error.error_exn (spf "Could not find action %s. Forgot define_action()?" 
        action_name) Not_found;
    BufferAction (fun _ -> ())
(*e: function [[Efuns.get_action]] *)

(*s: function [[Efuns.execute_action]] *)
let execute_action action_name = 
  match (get_action action_name) with
  | FrameAction f -> f 
  | BufferAction f -> (fun frame -> f frame.frm_buffer)
(*e: function [[Efuns.execute_action]] *)

(*s: function [[Efuns.execute_buffer_action]] *)
let execute_buffer_action action_name buf =
  match (get_action action_name) with
    BufferAction f -> f buf
  | FrameAction _f -> 
      Error.error (spf "Can't apply action %s on buffer" action_name)
(*e: function [[Efuns.execute_buffer_action]] *)

(*e: core/action.ml *)
