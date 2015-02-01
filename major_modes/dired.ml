(***********************************************************************)
(*                                                                     *)
(*                             ____                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Text
open Keymap
open Efuns
open Simple
open Compil
open Complex
open Window

let update buf =
  let filename = match buf.buf_filename with
      None -> failwith "Not a directory"
    | Some filename -> filename in
  let s = Utils.load_directory filename in
  let text = buf.buf_text in
  Text.update text s;
  buf.buf_last_saved <- Text.version text

let file_reg = Str.regexp ".* \([^ ]+\)$"
  
let get_file_line frame =
  (match frame.frm_buffer.buf_filename with
      None -> ()
    | Some filename -> 
        frame.frm_location.loc_dirname <- Filename.dirname filename);
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let start_point = Text.dup_point text point in
  let before = Text.point_to_bol text point in
  let after = Text.point_to_eol text point in
  Text.bmove text start_point before;
  let line = Text.sub text start_point (before + after) in
  Text.remove_point text start_point;
  line
    
let select_file line =
  if line.[0] = ' ' then
    if Str.string_match file_reg line 0 then
      Str.matched_group 1 line else
      String.sub line 60 (String.length line - 60)
  else
    failwith "Dired: not a file line"

let dirname frame =
  match frame.frm_buffer.buf_filename with
    None -> "."
  | Some dirname -> dirname 
      
let fullname frame filename = 
  Filename.concat (dirname frame) filename
      
let open_file frame =
  let filename = fullname frame (select_file (get_file_line frame)) in
  let location = frame.frm_location in
  let buf = Ebuffer.read location filename (Keymap.create ()) in
  let frame = Frame.create  frame.frm_window None buf in
  Frame.active frame
  
let remove frame =
  let line = get_file_line frame in   
  let filename = select_file line in
  let _ = 
    Select.select_yes_or_no frame (Printf.sprintf "Remove %s ? (y/n)" filename)
    (fun b -> if b then
          if line.[1] = 'd' then Unix.rmdir filename else
          Unix.unlink filename;
        update frame.frm_buffer) in
  ()

let view_list = ref []
let old_view_list = ref []
let compiled_view_list = ref []
  
let fast_view frame filename =
  if not (!old_view_list == !view_list) then
    begin
      compiled_view_list := List.map 
        (fun (file_reg, appli) ->
          Str.regexp file_reg, appli) !view_list;
      old_view_list := !view_list
    end;
  try
    List.iter (fun (regexp, viewer) ->
        if Str.string_match regexp filename 0 then
          try
            Unix.chdir (dirname frame);
            viewer frame filename;
            raise Exit
          with
            _ -> raise Exit
    ) !compiled_view_list;
    let _ = 
      Select.select_yes_or_no frame (Printf.sprintf "Open %s ? (y/n)" filename)
      (fun b -> if b then
            open_file frame) in
      ()

  with
    Exit -> ()      
  
let open_view frame =
  let filename = select_file (get_file_line frame) in
  fast_view frame filename
  
let mkdir frame =
  Select.select_filename frame "Make directory: "
    (fun str -> 
      let file_perm = try get_var frame.frm_buffer file_perm with _ -> 
            0x1ff land (lnot umask) in
      Unix.mkdir str file_perm;
      update frame.frm_buffer)
          
let install buf = 
  match buf.buf_filename with
    None -> 
      failwith "Dired: Not a directory"
  | Some filename ->
      if not (Utils.is_directory filename) then 
        failwith (Printf.sprintf "Dired: %s not a directory" filename);
      update buf
      
let mode = Ebuffer.new_major_mode "Dired" [install]

let map = mode.maj_map
  

let viewer commande frame filename =
  let _ =  Sys.command (Printf.sprintf "(%s %s) &" commande filename) in ()

let commande commande frame filename =
  let _ = Sys.command (Printf.sprintf commande filename) in
  failwith  (Printf.sprintf commande filename)
  
let unzip_and_view frame filename =
  let new_filename = Printf.sprintf "/tmp/efuns-view-%s" (
      Filename.chop_extension filename) in
  let res = Sys.command (
      Printf.sprintf "gzip -cd %s > %s" filename new_filename)
  in
  if res = 0 then fast_view frame new_filename
    
let _ = 
  interactive map [NormalMap, XK.xk_Return] "dired-open-file" open_file;
  interactive map [NormalMap, Char.code 'g'] "dired-update" 
  (fun frame -> update frame.frm_buffer);  
  interactive map [NormalMap, Char.code 'v'] "dired-view-file" open_view;  
  interactive map [NormalMap, Char.code '+'] "dired-make-directory" 
    mkdir;  
  interactive map [NormalMap, Char.code '-'] "dired-remove-entry" 
    remove;  
  
  view_list := [
    ".*\(\.jpg\|\..gig\|\.xpm\|\.ppm\)",viewer "xv";
    ".*\(\.ps\|\.PS\)",viewer "gv";
    ".*\(\.dvi\)",viewer "xdvi";
    ".*\(\.gz\|\.Z\|\.z\)",unzip_and_view; 
    ".*\.tgz", commande "xterm -e sh -c \"tar zvtf %s | less\"";
    ".*\.tar", commande "xterm -e sh -c \"tar vtf %s | less\"";
    ".*\.tar", commande "xterm -e sh -c \"tar vtf %s | less\"";
    ];
  
  Efuns.add_start_hook (fun location ->
      add_interactive (location.loc_map) "dired-mode" 
        (fun frame -> 
          Ebuffer.set_major_mode frame.frm_buffer mode);
      set_global location Ebuffer.modes_alist ((".*/$",mode) :: 
        (get_global location Ebuffer.modes_alist));      
  )   
