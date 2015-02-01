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

open Ocaml_mode
open Options
open Text
open Efuns
open Interactive
open Simple
open Select
open Compil
open Eval
open Complex
open Abbrevs  
open Env
open Types
open Keymap
open Window
open Location
open Compat_comp
open Type
open Ocaml_env
  
let type_buffer buf =
  let text = buf.buf_text in
  let start_point = Text.add_point text in
  let end_point = Text.add_point text in
  set_position text end_point (size text);
  let lexbuf = lexing text start_point end_point in
  try  
    let (str, env) = Type.type_buffer buf.buf_name lexbuf !!ocaml_path in
    remove_point text start_point;
    remove_point text end_point;
    (str,env)
  with x ->
      remove_point text start_point;
      remove_point text end_point;
      raise x

let compiled_idents = Local.create_abstr "compiled_idents"

let all_idents buf =
  try
    let (ids, version) = Local.get buf.buf_vars compiled_idents in
    if Text.version buf.buf_text = version then ids else raise Not_found
  with
    _ -> 
      let _, (str,env) = Utils.do_and_format type_buffer buf in
      let ids = Type.iter_structure str [] GlobalDefined in
      set_local buf compiled_idents (ids, Text.version buf.buf_text);
      ids      

let find_value_type sign names =
  let rec solv sign names =
    match names with
      [] -> raise Not_found
    | name :: names ->
        let rec find sign =
          match sign with
            [] -> raise Not_found
          | sg :: sign ->
              match sg with
                Tsig_value (ident,vd) 
                when Ident.name ident = name ->
                  vd.val_type
              | Tsig_module (ident,Tmty_signature sign ) 
                when Ident.name ident = name ->
                  solv sign names
              | _ -> find sign
        in
        find sign
  in
  solv sign names
    
let print_type frame = 
  let buf = frame.frm_buffer in
  let point = frame.frm_point in
  let top_window = Window.top frame.frm_window in
  try
    let ids = all_idents buf in
    let text = buf.buf_text in
    let pos = get_position text point in
    List.iter (fun (p,l,t,w) ->
        if l.loc_start-1 <= pos && l.loc_end+1 >= pos then
          Top_window.message top_window 
            (Utils.format_to_string 
              (fun () -> 
                printtyp_path p;
                Format.print_string " : ";
                printtyp_type_expr t
            ) ())
    ) ids
  with
    _ ->
  let name = find_long_word buf point in
  let names = parse_name name in
  let test_name modname names =
    try    
      let ps = Ocaml_env.find_pers_struct !!ocaml_path modname in
      let typ = find_value_type ps.ps_sig names in
      let styp = format_to_string () in
      Format.print_string modname;
      let rec iter list =
        match list with
          [] -> ()
        | name :: tail -> 
            Format.print_string ".";
            Format.print_string name;
            iter tail
      in
      iter names;
      Format.print_string ": ";
      printtyp_type_expr typ;
      Format.print_flush ();
      Top_window.message top_window !styp    
    with
      Env.Error e ->
        let m,s1,s2,s3 =
          match e with    
            Not_an_interface s -> "Not_an_interface",s,"",""
          | Corrupted_interface s -> "Corrupted_interface",s,"",""
          | Illegal_renaming (s1 , s2) ->
              "Illegal_renaming",s1,s2,"" 
          | Inconsistent_import (s1,s2,s3) ->
              "Inconsistent_import",s1,s2,s3
        in
        Top_window.message top_window 
          (Printf.sprintf "Env error: %s %s %s %s" m s1 s2 s3)
  in
  try
    match names with
      modname :: names when modname = String.capitalize modname -> 
        test_name modname names
    | _ -> raise Not_found
  with
    Not_found ->
      let rec iter env =
        match env with
          [] -> failwith "Can not find type"
        | modname :: env ->
            try
              test_name modname names
            with
              Not_found -> iter env
      in
      iter (find_env buf point)

let back_list = ref []
          
let find_implementation frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let word = find_long_word buf point in
  let ids = all_idents buf in
  let pos = get_position text point in
  try
    List.iter (fun (pp,l,t,w) ->
        if l.loc_start-1 <= pos && l.loc_end+1 >= pos then
          begin
            List.iter (fun (p,l,t,w) ->
                if p = pp && (w <> Type.Used)then
                  (Text.set_position text frame.frm_point l.loc_start;
                    raise Exit)
            ) ids;  
            match pp with
              Path.Pdot (Path.Pident id,name,pos) when Ident.persistent id ->
              (* Look in another file ... *)
                let ident = Ident.name id in
                let file = ident ^ ".ml" in
                file.[0] <- Char.lowercase file.[0];
                let filename = 
                  try
                    Utils.find_in_path !!ocaml_path file
                  with _ -> failwith (Printf.sprintf "No %s in path" file)
                in 
                let location = frame.frm_location in
                let buf = Ebuffer.read location filename (Keymap.create ()) in
                let text = buf.buf_text in
                let frame = try
                    Frame.find_buffer_frame location buf
                  with Not_found ->
                      Frame.create frame.frm_window None buf
                in
                Frame.active frame;
                let ids = all_idents buf in
                List.iter (fun (p,l,t,w) ->
                    match p with
                      Path.Pident id ->
                        if Ident.name id = name && w = Type.GlobalDefined then
                          (Text.set_position text frame.frm_point l.loc_start;
                            raise Exit)
                    | _ -> ()
                ) ids;
            | _  -> ()
          end
    
    ) ids;
  with
    Exit -> 
      let filename = 
        match buf.buf_filename with
          None -> buf.buf_name
        | Some filename -> filename
      in
      back_list := (filename,Text.get_position text point) :: !back_list

let rec backward_implementation frame =
  match !back_list with
    [] -> failwith "No more buffers in history"
  | (filename, pos) :: tail ->
      back_list := tail;
      let location = frame.frm_location in
      let buf = Ebuffer.read location filename (Keymap.create ()) in
      let frame = Frame.create frame.frm_window None buf in
      Frame.active frame
      
let mouse_find_implementation frame =
  let frame = Top_window.mouse_set_active (Window.top frame.frm_window) in
  find_implementation frame
  
  

let mode = Ebuffer.new_minor_mode "compiler" []

let local_map = define_option ["ocaml_compiler_mode"; "local_map"] ""
    (list_option binding_option) []
let _ = 
  if !!local_map = [] then 
    local_map =:= [
      [c_c; ControlMap, Char.code 'i'] , "ocaml_mode.find_implementation";
      [c_c; ControlMap, XK.xk_BackSpace], "ocaml_mode.backward_implementation";
      [c_c; ControlMap, Char.code 't'], "ocaml_mode.print_type";
    ];
  Keymap.add_binding mode.min_map [ControlMap, XK.xk_Pointer_Button1]
    mouse_find_implementation

let _ = 
  define_buffer_action "ocaml_compiler_mode" 
    (fun buf -> 
      if Ebuffer.modep buf mode then begin
          Ebuffer.del_minor_mode buf mode
        end else
        Ebuffer.set_minor_mode buf mode);
  define_action "ocaml_compiler_mode.find_implementation" find_implementation;
  define_action "ocaml_compiler_mode.backward_implementation" backward_implementation;
  define_action "ocaml_compiler_mode.print_type" print_type;

    
    