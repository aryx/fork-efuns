(*s: core/keymap.ml *)
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

(*s: type [[Keymap.t]] *)
type t = Efuns.map
(*e: type [[Keymap.t]] *)

(*s: function [[Keymap.dummy_action]] *)
let dummy_action _frame = () 
(*e: function [[Keymap.dummy_action]] *)

(*s: function [[Keymap.create]] *)
let create () =
  { char_map = Array.make 256 Unbound;
    complex_bindings = [];
  } 
(*e: function [[Keymap.create]] *)


(*s: function [[Keymap.print_key]] *)
let print_key (map,keysym) =
  let prefix =
    match map with
    | NormalMap -> ""
    | ControlMap -> "C-"
    | MetaMap -> "M-"
    | ControlMetaMap -> "CM-"
  in
  let kname =
    try List.assoc keysym XK.keysym_to_name
    with Not_found -> 
      try Printf.sprintf "%c" (Char.chr keysym)
      with Invalid_argument _ -> "?"
  in
  prefix^kname
(*e: function [[Keymap.print_key]] *)
            
(*s: function [[Keymap.print_key_list]] *)
let rec print_key_list key_list =
  match key_list with
  | [] -> ""
  | [key] -> Printf.sprintf "%s" (print_key key)
  | key :: tail ->
      Printf.sprintf "%s %s" (print_key key) (print_key_list tail)
(*e: function [[Keymap.print_key_list]] *)
   

(*s: function [[Keymap.get_binding]] *)
let rec get_binding map keylist =
  match keylist with
    [] -> Unbound
  | [key] ->
        (*s: [[Keymap.get_binding()]] find key in map *)
        (match key with
          (NormalMap,key) when key >= 0 && key < 256 -> 
            map.char_map.(key)
        | _ -> 
            try List.assoc key map.complex_bindings
            with Not_found -> Unbound
         )
        (*e: [[Keymap.get_binding()]] find key in map *)
  | key :: tail ->
      match
        (*s: [[Keymap.get_binding()]] find key in map *)
        (match key with
          (NormalMap,key) when key >= 0 && key < 256 -> 
            map.char_map.(key)
        | _ -> 
            try List.assoc key map.complex_bindings
            with Not_found -> Unbound
         )
        (*e: [[Keymap.get_binding()]] find key in map *)
      with
      | Prefix map -> get_binding map tail
      | _ -> Unbound
(*e: function [[Keymap.get_binding]] *)
          
(*s: function [[Keymap.set_binding]] *)
let set_binding map key binding =
  match key with
  | (NormalMap,key) when key >= 0 && key < 256 -> 
      map.char_map.(key) <- binding
  | _ -> 
      map.complex_bindings <- (key,binding) :: map.complex_bindings
(*e: function [[Keymap.set_binding]] *)

(*s: function [[Keymap.add_binding]] *)
let rec add_binding map key_list binding =
  match key_list with
    [] -> failwith "ERROR add_complex_binding: empty key list"
  | [key] -> set_binding map key (Function binding)
  | key :: tail ->
      match
        match key with
          (NormalMap,key) when key >= 0 && key < 256 -> 
            map.char_map.(key)
        | _ -> 
            try
              List.assoc key map.complex_bindings
            with
              Not_found -> Unbound
      with
        Prefix map -> add_binding map tail binding
      | Unbound ->
          let newmap = create () in
          set_binding map key (Prefix newmap);
          add_binding newmap tail binding;
      | _e -> 
          failwith "ERROR add_complex_binding: Unable to add prefix"
(*e: function [[Keymap.add_binding]] *)
          
   
(*s: constant [[Keymap.c_h]] *)
let c_h = (ControlMap, Char.code 'h')
(*e: constant [[Keymap.c_h]] *)
(*s: constant [[Keymap.c_x]] *)
let c_x = (ControlMap, Char.code 'x')
(*e: constant [[Keymap.c_x]] *)
(*s: constant [[Keymap.c_c]] *)
let c_c = (ControlMap, Char.code 'c')
(*e: constant [[Keymap.c_c]] *)

(*s: function [[Keymap.all_bindings]] *)
let all_bindings () =
  let s = ref "Default bindings:" in
  (* TODO *)
  !s
(*e: function [[Keymap.all_bindings]] *)
  

(*s: function [[Keymap.add_global_key]] *)
let add_global_key prefix action =
  add_binding (Globals.editor()).edt_map prefix action
(*e: function [[Keymap.add_global_key]] *)
(*s: function [[Keymap.add_local_key]] *)
let add_local_key buf prefix action = 
  add_binding buf.buf_map prefix action
(*e: function [[Keymap.add_local_key]] *)
(*s: function [[Keymap.add_minor_key]] *)
let add_minor_key minor prefix action = 
  add_binding minor.min_map prefix action
(*e: function [[Keymap.add_minor_key]] *)
(*s: function [[Keymap.add_major_key]] *)
let add_major_key major prefix action = 
  add_binding major.maj_map prefix action
(*e: function [[Keymap.add_major_key]] *)

(*****************************************************************************)
(* Keys <-> string *)
(*****************************************************************************)

(*s: function [[Simple.string_to_modifier]] *)
let string_to_modifier s =  
  let mask = ref 0 in
  for i = 0 to String.length s - 1 do
    mask := !mask lor (match s.[i] with
      | 'C' -> Xtypes.controlMask
      | 'A' -> Xtypes.mod1Mask
      | 'M' -> Xtypes.mod1Mask
      | '1' -> Xtypes.mod1Mask
      | _ -> 0
    )
  done;
  !mask
(*e: function [[Simple.string_to_modifier]] *)
  
(*s: constant [[Simple.name_to_keysym]] *)
let name_to_keysym = 
  ("Button1", XK.xk_Pointer_Button1) ::
  ("Button2", XK.xk_Pointer_Button2) ::
  ("Button3", XK.xk_Pointer_Button3) ::
  ("Button4", XK.xk_Pointer_Button4) ::
  ("Button5", XK.xk_Pointer_Button5) ::
  XK.name_to_keysym
(*e: constant [[Simple.name_to_keysym]] *)
  
(*s: function [[Simple.value_to_key]] *)
(* form: SC-Button1 *)
let value_to_key v =
  match v with 
    Options.Value s -> 
      let key, mods = 
        try
          let index = String.index s '-' in
          let mods = String.sub s 0 index in
          let key = String.sub s (index+1) (String.length s - index - 1) in
          key, mods
        with _ -> s, ""
      in
      let key = List.assoc key name_to_keysym in
      let mods = string_to_modifier mods in
      let map = 
        if mods land (Xtypes.controlMask lor Xtypes.mod1Mask) = 
                     (Xtypes.controlMask lor Xtypes.mod1Mask)
        then ControlMetaMap else
        if mods land Xtypes.controlMask <> 0 then ControlMap else
        if mods land Xtypes.mod1Mask <> 0 then MetaMap else NormalMap
      in
      map, key
      
  | _ -> raise Not_found
(*e: function [[Simple.value_to_key]] *)
  
(*s: function [[Simple.key_to_value]] *)
let key_to_value k = Options.Value (print_key k)
(*e: function [[Simple.key_to_value]] *)
      
(*s: constant [[Simple.key_option]] *)
let key_option = Options.define_type "Key" value_to_key key_to_value
(*e: constant [[Simple.key_option]] *)

(*s: constant [[Simple.binding_option]] *)
let binding_option = 
  Options.tuple2_option (Options.smalllist_option key_option, 
                         Options.string_option)
(*e: constant [[Simple.binding_option]] *)

(*e: core/keymap.ml *)
