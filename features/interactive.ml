(*s: features/interactive.ml *)
(*s: copyright header2 *)
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
(*e: copyright header2 *)
open Efuns

(* M-x *)
  
(*s: function [[Interactive.create_bindings]] *)
let create_bindings () =
  let s = Keymap.all_bindings () in
  let text = Text.create s in
  Ebuffer.create "*bindings*" None text (Keymap.create ())
(*e: function [[Interactive.create_bindings]] *)

(*s: constant [[Interactive.meta_hist]] *)
let meta_hist = ref []
(*e: constant [[Interactive.meta_hist]] *)

(*s: function [[Interactive.buf_interactives]] *)
let buf_interactives buf =
  let interactives = 
    buf.buf_major_mode.maj_map.interactives @
    (Globals.editor()).edt_map.interactives 
  in
  List.fold_left (fun list minor -> minor.min_map.interactives @ list) 
   interactives buf.buf_minor_modes 
(*e: function [[Interactive.buf_interactives]] *)
  
(*s: function [[Interactive.exec_interactive]] *)
let exec_interactive interactives frame name =
  try
    let f, key = List.assoc name interactives in
    (*s: [[Interactive.exec_interactive()]] display if has a keybinding *)
    key |> Common.do_option (fun key_list ->
      let top_window = Window.top frame.frm_window in
      Top_window.message top_window
        ("you can run "^name^" by typing "^
          (Keymap.print_key_list key_list))
    );
    (*e: [[Interactive.exec_interactive()]] display if has a keybinding *)
    (* run it ! *)
    f frame
  with Not_found -> 
    let top_window = Window.top frame.frm_window in
    Top_window.message top_window ("No interactive command "^name)
(*e: function [[Interactive.exec_interactive]] *)
      
(*s: function [[Interactive.call_interactive]] *)
let call_interactive frame =
  let buf = frame.frm_buffer in
  let interactives = buf_interactives buf in
  Select.select frame "M-x " meta_hist "" 
    (fun _ -> List.map fst interactives)
    (fun s -> s) 
    (fun s -> exec_interactive interactives frame s)
[@@interactive]
(*e: function [[Interactive.call_interactive]] *)

(* variables *)

(*s: constant [[Complex.variable_hist]] *)
let variable_hist = ref []
(*e: constant [[Complex.variable_hist]] *)
(*s: constant [[Complex.value_hist]] *)
let value_hist = ref []
(*e: constant [[Complex.value_hist]] *)
  
(*s: constant [[Complex.all_vars]] *)
let all_vars = ref None
(*e: constant [[Complex.all_vars]] *)
(*s: function [[Complex.all_variables]] *)
let all_variables frame _ =
  let buf = frame.frm_buffer in
  match !all_vars with
    Some (f,l) when f == frame -> l
  | _ ->
      let list = 
        (Store.list buf.buf_vars) @ 
        (Store.list (Globals.editor()).edt_vars) 
      in
      all_vars := Some (frame, list);
      list
(*e: function [[Complex.all_variables]] *)
  
(*s: function [[Complex.set_local_variable]] *)
let set_local_variable frame = 
  Select.select frame "set_local_variable : " variable_hist
    "" (all_variables frame) (fun s -> s) (fun variable ->
      Select.select_string frame (Printf.sprintf "%s : " variable)
      value_hist "" (fun value ->
          Store.input frame.frm_buffer.buf_vars variable value))
(*e: function [[Complex.set_local_variable]] *)
  
(*s: function [[Complex.set_global_variable]] *)
let set_global_variable frame =
  Select.select frame "set_global_variable : " variable_hist
    "" (all_variables frame) (fun s -> s) (fun variable ->
      Select.select_string frame (Printf.sprintf "%s : " variable)
      value_hist "" (fun value ->
          Store.input (Globals.editor()).edt_vars variable value))
(*e: function [[Complex.set_global_variable]] *)
  
(*s: function [[Complex.get_variable]] *)
let describe_variable frame = 
  Select.select frame "get_variable : " variable_hist "" 
    (all_variables frame)
    (fun s -> s) 
    (fun variable ->
      Top_window.mini_message frame 
        (Printf.sprintf "%s : %s" variable (
          let buf = frame.frm_buffer in
          try
            Store.print buf.buf_vars variable
          with _ ->
            Store.print (Globals.editor()).edt_vars variable)))
[@@interactive]
(*e: function [[Complex.get_variable]] *)

(*e: features/interactive.ml *)
