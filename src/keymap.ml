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

(*
La gestion des keymaps est inadapte'e a` la modification dynamique.
En effet, on aurait envie de pouvoir modifier les bindings selon divers
  crite`res, tels que le buffer (possible), le mode(pas possible).
*)


open Efuns

let dummy_action frame = () 
let unbound_key buffer _ = raise UnboundKey

let create () =
  { char_map = Array.create 256 Unbound;
    complex_bindings = [];
    interactives = [];
  } 

let print_key (map,keysym) =
  let prefix =
    match map with
      NormalMap -> ""
    | ControlMap -> "C-"
    | MetaMap -> "M-"
    | ControlMetaMap -> "CM-"
  in
  let kname =
    try
      List.assoc keysym XK.keysym_to_name
    with
      Not_found -> "?"
  in
  prefix^kname
            
let rec print_key_list key_list =
  match key_list with
    key :: tail ->
      Printf.sprintf "%s %s" (print_key key) (print_key_list tail)
  | _ -> ""

let print map =
  Printf.printf "MAP:\n";
  List.iter (fun (key, binding) ->
    (match binding with
      Prefix _ -> print_string "Prefix "; 
    | Function _ -> print_string "Function"
    | Unbound -> print_string "Unbound"
          );
    print_string (print_key key);
    print_newline ()
      ) map.complex_bindings
    
let rec get_binding map keylist =
  match keylist with
    [] -> Unbound
  | [key] ->
      begin
        match key with
          (NormalMap,key) when key >= 0 && key < 256 -> 
            map.char_map.(key)
        | _ -> 
            try
              List.assoc key map.complex_bindings
            with
              Not_found -> Unbound
      end
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
        Prefix map -> get_binding map tail
      | _ -> Unbound
          
          
let set_binding map key binding =
  match key with
    (NormalMap,key) when key >= 0 && key < 256 -> 
      map.char_map.(key) <- binding
  | _ -> 
      map.complex_bindings <- (key,binding) :: map.complex_bindings

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
          

let define_char_binding map char (f : frame -> 'a) =
  add_binding map [NormalMap,Char.code char] f
    
let c_h = (ControlMap, Char.code 'h')
let c_x = (ControlMap, Char.code 'x')
let c_c = (ControlMap, Char.code 'c')
let n_5 = (NormalMap, Char.code '5')

let all_bindings location =
  let s = ref "Default bindings:" in
  List.iter (fun (name,(_,binding)) ->
      match binding with
        None -> ()
      | Some key_list ->
          s := Printf.sprintf "%s\n%s : %s" !s 
            (print_key_list (List.rev key_list)) name
  ) location.loc_map.interactives;
  !s
  
let interactive map keylist name f =
  map.interactives <- (name, (f, Some keylist)) :: map.interactives;
  add_binding map keylist f

let add_interactive map name f =
    map.interactives <- (name, (f, None)) :: map.interactives

let add_global_key location = interactive location.loc_map 
let add_local_key buf = interactive buf.buf_map 
let add_minor_key minor = interactive minor.min_map 
let add_major_key major = interactive major.maj_map 
  