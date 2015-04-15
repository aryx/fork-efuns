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

(*s: function Keymap.dummy_action *)
let dummy_action frame = () 
(*e: function Keymap.dummy_action *)

(*s: function Keymap.create *)
let create () =
  { char_map = Array.create 256 Unbound;
    complex_bindings = [];
    interactives = [];
  } 
(*e: function Keymap.create *)


(*s: function Keymap.print_key *)
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
(*e: function Keymap.print_key *)
            
(*s: function Keymap.print_key_list *)
let rec print_key_list key_list =
  match key_list with
  | [] -> ""
  | [key] -> Printf.sprintf "%s" (print_key key)
  | key :: tail ->
      Printf.sprintf "%s %s" (print_key key) (print_key_list tail)
(*e: function Keymap.print_key_list *)
   

(*s: function Keymap.get_binding *)
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
(*e: function Keymap.get_binding *)
          
(*s: function Keymap.set_binding *)
let set_binding map key binding =
  match key with
  | (NormalMap,key) when key >= 0 && key < 256 -> 
      map.char_map.(key) <- binding
  | _ -> 
      map.complex_bindings <- (key,binding) :: map.complex_bindings
(*e: function Keymap.set_binding *)

(*s: function Keymap.add_binding *)
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
      | e -> 
          failwith "ERROR add_complex_binding: Unable to add prefix"
(*e: function Keymap.add_binding *)
          
   
(*s: constant Keymap.c_h *)
let c_h = (ControlMap, Char.code 'h')
(*e: constant Keymap.c_h *)
(*s: constant Keymap.c_x *)
let c_x = (ControlMap, Char.code 'x')
(*e: constant Keymap.c_x *)
(*s: constant Keymap.c_c *)
let c_c = (ControlMap, Char.code 'c')
(*e: constant Keymap.c_c *)
(*s: constant Keymap.n_5 *)
let n_5 = (NormalMap, Char.code '5')
(*e: constant Keymap.n_5 *)

(*s: function Keymap.all_bindings *)
let all_bindings () =
  let s = ref "Default bindings:" in
  (Globals.location()).loc_map.interactives |> List.iter(fun (name,(_,binding)) ->
    match binding with
    | None -> ()
    | Some key_list ->
        s := Printf.sprintf "%s\n%20s : %s" !s 
              (print_key_list key_list) name
  );
  !s
(*e: function Keymap.all_bindings *)
  
(*s: function Keymap.interactive *)
let interactive map =
 fun keylist name f ->
  (*s: [[Keymap.interactive()]] add keylist and name to interactives list *)
  map.interactives <- (name, (f, Some keylist)) :: map.interactives;
  (*e: [[Keymap.interactive()]] add keylist and name to interactives list *)
  add_binding map keylist f
(*e: function Keymap.interactive *)

(*s: function Keymap.add_interactive *)
let add_interactive map name f =
    map.interactives <- (name, (f, None)) :: map.interactives
(*e: function Keymap.add_interactive *)

(*s: function Keymap.add_global_key *)
let add_global_key = fun prefix string action ->
  interactive (Globals.location()).loc_map prefix string action
(*e: function Keymap.add_global_key *)
(*s: function Keymap.add_local_key *)
let add_local_key buf = 
  interactive buf.buf_map 
(*e: function Keymap.add_local_key *)
(*s: function Keymap.add_minor_key *)
let add_minor_key minor = 
  interactive minor.min_map 
(*e: function Keymap.add_minor_key *)
(*s: function Keymap.add_major_key *)
let add_major_key major = 
  interactive major.maj_map 
(*e: function Keymap.add_major_key *)

(*s: function Keymap.define_interactive_action *)
let define_interactive_action action_name action_fun =
  Action.define_action action_name action_fun;
  let map = (Globals.location()).loc_map in
  add_interactive map action_name action_fun
(*e: function Keymap.define_interactive_action *)

(*e: core/keymap.ml *)
