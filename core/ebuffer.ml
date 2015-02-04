(*s: core/ebuffer.ml *)
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

open Utils
open Efuns
open Text

(*s: constant Ebuffer.create_buf_hook *)
let create_buf_hook = Local.create_abstr "create_buf_hook"
(*e: constant Ebuffer.create_buf_hook *)
(*s: constant Ebuffer.modes_alist *)
let modes_alist = Local.create_abstr "modes_alist"
(*e: constant Ebuffer.modes_alist *)

(*s: function Ebuffer.create_syntax_table *)
let create_syntax_table ()  =
  let table = Array.create 256 false 
  in  
  for i = Char.code 'a' to Char.code 'z' do
    table.(i) <- true;
  done;
  for i = Char.code 'A' to Char.code 'Z' do
    table.(i) <- true;
  done;
  for i = Char.code '0' to Char.code '9' do
    table.(i) <- true;
  done;
  table
(*e: function Ebuffer.create_syntax_table *)

(*s: constant Ebuffer.default_syntax_table *)
let default_syntax_table = create_syntax_table ()
(*e: constant Ebuffer.default_syntax_table *)

(*s: function Ebuffer.get_name *)
let get_name location filename =
  let basename = Filename.basename filename in
  let name = 
    if basename = "" then
      (Filename.basename (Filename.dirname filename)) ^ "/"
    else
      basename
  in
  let i = ref 0 in
  let compute_name () =
    if !i = 0 
    then name 
    else Printf.sprintf "%s<%d>" name !i
  in
  try
    while true do
      let _ = Hashtbl.find location.loc_buffers (compute_name ()) in 
      incr i
    done; assert false
  with
    Not_found -> 
      compute_name ()
(*e: function Ebuffer.get_name *)

      
(*s: function Ebuffer.new_minor_mode *)
let new_minor_mode name = {
    min_name = name;
    min_map = Keymap.create ();
    min_hooks = [];
    min_vars = Local.vars ()
  }
(*e: function Ebuffer.new_minor_mode *)
      
(*s: function Ebuffer.new_minor_mode (core/ebuffer.ml) *)
let new_minor_mode name hooks  = {
    min_name = name;
    min_map = Keymap.create ();
    min_hooks = hooks;
    min_vars = Local.vars ()
  }
(*e: function Ebuffer.new_minor_mode (core/ebuffer.ml) *)

(*s: function Ebuffer.new_major_mode *)
let new_major_mode name hooks = {
    maj_name = name;
    maj_map = Keymap.create ();
    maj_hooks = hooks;
    maj_vars = Local.vars ();
  }
(*e: function Ebuffer.new_major_mode *)

(*s: constant Ebuffer.fondamental_mode *)
let fondamental_mode = new_major_mode "Fondamental" []
(*e: constant Ebuffer.fondamental_mode *)
  
(*s: constant Ebuffer.tab_size *)
let tab_size = ref 9
(*e: constant Ebuffer.tab_size *)

(*s: function Ebuffer.create *)
let create location name filename text local_map =
  let name = get_name location name in
  let buf =
    { 
      buf_text = text;
      buf_name = name;
      buf_filename = filename;

      buf_modified = 0;
      buf_last_saved = version text;
      buf_history = [];
      buf_charreprs = Array.init 256 (fun i ->   String.make 1 (Char.chr i));
      buf_map_partial = true;
      buf_map = local_map;
      buf_sync = false;
      buf_mark = None;
      buf_point = Text.add_point text;
      buf_start = Text.add_point text;
      buf_shared = 0;
      buf_syntax_table = default_syntax_table;
      buf_finalizers = [];
      buf_vars = Local.vars ();
      buf_location = location;
      buf_minor_modes = [];
      buf_major_mode = fondamental_mode;
    } in
  Hashtbl.add location.loc_buffers name buf;

  for i=0 to 25 do
    let s = String.make 2 '^' in
    s.[1] <- Char.chr (97+i);    
    buf.buf_charreprs.(i) <- s
  done;
  buf.buf_charreprs.(9) <- String.make !tab_size ' ';

  let hooks =  try
      get_global location create_buf_hook
    with Not_found -> []
  in
  exec_hooks hooks buf;
  buf
(*e: function Ebuffer.create *)

(*s: function Ebuffer.kill *)
let kill location buf =
  Hashtbl.remove location.loc_buffers buf.buf_name;
  begin
    match buf.buf_filename with
      None -> ()
    | Some filename ->
        Hashtbl.remove location.loc_files filename
  end;
  List.iter (fun f -> f () ) buf.buf_finalizers;
  Gc.compact ();
  buf.buf_shared <- -1
(*e: function Ebuffer.kill *)

open Options
  
(*s: constant Ebuffer.save_buffer_hooks *)
let save_buffer_hooks = define_option ["save_buffer_hooks"] "" 
    (list_option string_option)
  [ ]
(*e: constant Ebuffer.save_buffer_hooks *)
  
(*s: constant Ebuffer.saved_buffer_hooks *)
let saved_buffer_hooks = define_option ["saved_buffer_hooks"] "" 
    (list_option string_option)
  ["update_time" ]
(*e: constant Ebuffer.saved_buffer_hooks *)

(*s: function Ebuffer.exec_named_buf_hooks *)
let rec exec_named_buf_hooks hooks frame =
  match hooks with
    [] -> ()
  | action :: hooks ->
      exec_named_buf_hooks hooks frame;
      try execute_buffer_action action frame with _ -> ()
(*e: function Ebuffer.exec_named_buf_hooks *)

(*s: function Ebuffer.exec_named_buf_hooks_with_abort *)
let rec exec_named_buf_hooks_with_abort hooks frame =
  match hooks with
    [] -> ()
  | action :: hooks ->
      exec_named_buf_hooks_with_abort hooks frame;
      execute_buffer_action action frame
(*e: function Ebuffer.exec_named_buf_hooks_with_abort *)
      
(*s: function Ebuffer.save *)
let save buf =
  exec_named_buf_hooks_with_abort !!saved_buffer_hooks buf;
  let filename =
    match buf.buf_filename with
      None -> raise Not_found
    | Some name -> name
  in
  let outc = open_out filename in
  Text.save buf.buf_text outc;
  close_out outc;
  buf.buf_last_saved <- version buf.buf_text;
  exec_named_buf_hooks !!saved_buffer_hooks buf
(*e: function Ebuffer.save *)


(*s: exception Ebuffer.Found *)
exception Found of buffer
(*e: exception Ebuffer.Found *)

  
(*s: function Ebuffer.read *)
let read location filename local_map =
  try
    let filename = Utils.normal_name location.loc_dirname filename in
    try
      Hashtbl.find location.loc_files filename
    with
      Not_found ->
        let text =
          try
            let inc = open_in filename in
            let text = Text.read inc in         
            close_in inc; 
            text
          with
            _ -> Text.create ""
        in
        let buf = create location filename (Some filename) text local_map in
        Hashtbl.add location.loc_files filename buf;
        buf
  with
    Found buf -> buf
(*e: function Ebuffer.read *)

(*s: function Ebuffer.default *)
let default location name =
  failwith "TODO"
(*
  try
    Hashtbl.find location.loc_buffers name
  with
    Not_found ->
      let str = 
        if name = "*help*" then
          "Welcome to Efuns, a small demo editor written in Ocaml.

Version is " ^ Version.efuns_version ^"
built by "^ Version.builder ^ " " ^ Version.date ^ " 
with
Efuns installation directory : " ^ Version.efuns_lib ^ "
Ocaml installation directory : " ^ Version.ocamllib ^ "

Fabrice Le Fessant
PARA/SOR Project
INRIA Rocquencourt

Help for Key Bindings: C-h K
See changes in "^ Version.efuns_lib ^"/Changes
"
        else ""
      in
      create location name None (Text.create str) (Keymap.create ())
*)
(*e: function Ebuffer.default *)
      

(*s: function Ebuffer.compute_representation *)
let compute_representation buf n =
  Text.compute_representation buf.buf_text buf.buf_charreprs n
(*e: function Ebuffer.compute_representation *)

(*s: exception Ebuffer.BufferAlreadyOpened *)
exception BufferAlreadyOpened
(*e: exception Ebuffer.BufferAlreadyOpened *)

(*s: function Ebuffer.change_name *)
let change_name location buf filename =
  Hashtbl.remove location.loc_buffers buf.buf_name;
  (match buf.buf_filename with
      None -> ()
    | Some filename ->
        Hashtbl.remove location.loc_files filename);
  let filename = 
    if Filename.is_relative filename then
      Filename.concat location.loc_dirname filename
    else
      filename
  in
  if hashtbl_mem location.loc_files filename then
    raise BufferAlreadyOpened;
  let filename = Utils.normal_name location.loc_dirname filename in
  let name = get_name location filename in
  Hashtbl.add location.loc_buffers name buf;
  Hashtbl.add location.loc_files filename buf;
  buf.buf_filename <- Some filename;
  buf.buf_name <- name
(*e: function Ebuffer.change_name *)
  
  
(*s: function Ebuffer.set_mark *)
let set_mark buf point =
  let text = buf.buf_text in
  buf.buf_modified <- buf.buf_modified + 1;
  match buf.buf_mark with
    None ->
      let mark = dup_point text point in
      buf.buf_mark <- Some mark
  | Some mark ->
      goto_point text mark point
(*e: function Ebuffer.set_mark *)

(*s: function Ebuffer.get_mark *)
let rec get_mark buf point =
  match buf.buf_mark with
    None -> 
      set_mark buf point;
      get_mark buf point
  | Some mark -> mark
(*e: function Ebuffer.get_mark *)

(*s: function Ebuffer.remove_mark *)
let remove_mark buf =
  match buf.buf_mark with
    None -> ()
  | Some mark ->
      buf.buf_mark <- None;
      remove_point buf.buf_text mark;
      buf.buf_modified <- buf.buf_modified + 1
(*e: function Ebuffer.remove_mark *)

(*s: constant Ebuffer.modes_old *)
let modes_old = ref []
(*e: constant Ebuffer.modes_old *)
(*s: constant Ebuffer.regexp_alist *)
let regexp_alist = ref []
(*e: constant Ebuffer.regexp_alist *)

(*s: function Ebuffer.set_major_mode *)
let set_major_mode buf mode =
  buf.buf_modified <- buf.buf_modified + 1;
  buf.buf_major_mode <- mode;
  List.iter (fun f -> 
      try f buf with _ -> ()) mode.maj_hooks
(*e: function Ebuffer.set_major_mode *)

(*s: function Ebuffer.set_minor_mode *)
let set_minor_mode buf mode =
  buf.buf_minor_modes <- mode :: buf.buf_minor_modes;
  buf.buf_modified <- buf.buf_modified + 1;
  List.iter (fun f -> 
      try f buf with _ -> ()) mode.min_hooks
(*e: function Ebuffer.set_minor_mode *)

(*s: function Ebuffer.del_minor_mode *)
let del_minor_mode buf minor =
  buf.buf_minor_modes <- 
    List.fold_right 
    (fun mode list -> 
      if mode == minor then
        begin
          buf.buf_modified <- buf.buf_modified + 1;
          list
        end
      else (mode :: list)) buf.buf_minor_modes []
(*e: function Ebuffer.del_minor_mode *)
  
(*s: function Ebuffer.modep *)
let modep buf minor =
  List.memq minor buf.buf_minor_modes
(*e: function Ebuffer.modep *)

(*s: constant Ebuffer.suffix_reg *)
let suffix_reg = Str.regexp "\(.*\)<[0-9]+>$"
(*e: constant Ebuffer.suffix_reg *)
  
(*s: function Ebuffer.set_buffer_mode *)
let set_buffer_mode buf =
  let buf_name = 
    match buf.buf_filename with
      None -> 
        (try
          if Str.string_match suffix_reg buf.buf_name 0 then
            Str.matched_group 1 buf.buf_name else buf.buf_name 
        with
          _ -> buf.buf_name)
    | Some file_name -> file_name 
  in 
  let modes_alist = get_var buf modes_alist in
  if not (!modes_old == modes_alist) then
    begin
      regexp_alist := 
      List.map 
        (fun (file_reg, major) ->
          Str.regexp file_reg, major) modes_alist;
      modes_old := modes_alist;
    end;
  try
    List.iter (fun (regexp, major) ->
        if Str.string_match regexp buf_name 0 then
          try
            set_major_mode buf major;
            raise Exit
          with
            _ -> raise Exit
    ) !regexp_alist
  with
    Exit -> ()
(*e: function Ebuffer.set_buffer_mode *)
      
(*s: function Ebuffer.get_binding *)
let get_binding buf keylist =
  let binding = ref Unbound in
  try
    (*s: [[Ebuffer.get_binding()]] minor mode key search *)
    List.iter (fun minor ->
        let b = Keymap.get_binding minor.min_map keylist in
        match b with
          Prefix map -> binding := b
        | Function f -> binding := b; raise Exit
        | Unbound -> ()
    ) buf.buf_minor_modes; 
    (*e: [[Ebuffer.get_binding()]] minor mode key search *)
    (*s: [[Ebuffer.get_binding()]] major mode key search *)
    (let b = Keymap.get_binding buf.buf_major_mode.maj_map keylist in
      match b with
        Prefix map -> binding := b
      | Function f -> binding := b; raise Exit
      | Unbound -> ());
    (*e: [[Ebuffer.get_binding()]] major mode key search *)
    (let b = Keymap.get_binding buf.buf_map keylist in
      match b with
        Prefix map -> binding := b;
      | Function f -> binding := b; raise Exit
      | Unbound -> ());
    (*s: [[Ebuffer.get_binding()]] if partial map *)
    if buf.buf_map_partial then
      (let b = Keymap.get_binding buf.buf_location.loc_map keylist in
        match b with
          Prefix map -> binding := b;
        | Function f -> binding := b; raise Exit
        | Unbound -> ());
    (*e: [[Ebuffer.get_binding()]] if partial map *)
    !binding
  with
    Exit -> !binding
(*e: function Ebuffer.get_binding *)

(*s: function Ebuffer.message *)
let message buf m =
  let location = buf.buf_location in
  let name = "*Messages*" in
  try
    let buf = Hashtbl.find location.loc_buffers name in
    Text.insert_at_end buf.buf_text (m ^ "\n");
  with
    Not_found ->
      let buf = create location name None (Text.create (m^"\n")) (
          Keymap.create ())
      in ()
(*e: function Ebuffer.message *)

(*s: function Ebuffer.catch *)
let catch format buf f =
  try
    f ()
  with e ->
      let location = buf.buf_location in
      let name = "*Messages*" in
      let m = Printf.sprintf format (Utils.printexn e) in
      try
        let buf = Hashtbl.find location.loc_buffers name in
        Text.insert_at_end buf.buf_text (m ^ "\n");
      with
        Not_found ->
          let buf = create location name None (Text.create (m^"\n")) (
              Keymap.create ())
          in ()
(*e: function Ebuffer.catch *)
          
      
(*s: toplevel Ebuffer._1 *)
let _ =
  Efuns.add_start_hook 
    (fun location ->
      set_global location create_buf_hook [set_buffer_mode];
      set_global location modes_alist []
      )
(*e: toplevel Ebuffer._1 *)
(*e: core/ebuffer.ml *)
