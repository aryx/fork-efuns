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
    (Globals.editor()).loc_map.interactives 
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
(*e: function [[Interactive.call_interactive]] *)
(*e: features/interactive.ml *)
