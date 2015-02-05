(*s: misc/efuns_filebrowser.ml *)
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

open Xtypes
open WX_types
open WX_tree
open Unix

(*s: constant Efuns_filebrowser.argv *)
let argv = Array.to_list Sys.argv
(*e: constant Efuns_filebrowser.argv *)
(*s: constant Efuns_filebrowser.regexps *)
let regexps = List.map (fun str -> str, 
      Str.regexp (Utils.glob_to_regexp str)) (
    match argv with
      program :: dirname :: strs -> strs
    | _ -> failwith "Usage: efuns_filebrowser dirname [regexps]")
(*e: constant Efuns_filebrowser.regexps *)

(*s: constant Efuns_filebrowser.dirname *)
let dirname = ref (if Filename.is_relative Sys.argv.(1) then
      Filename.concat (Sys.getcwd ()) Sys.argv.(1) else Sys.argv.(1))
(*e: constant Efuns_filebrowser.dirname *)
  
(*s: constant Efuns_filebrowser.display *)
let display = new WX_display.t ""
(*e: constant Efuns_filebrowser.display *)
(*s: constant Efuns_filebrowser.root *)
let root = new WX_root.t display 0
(*e: constant Efuns_filebrowser.root *)
(*s: constant Efuns_filebrowser.top *)
let top = new WX_appli.t root [MinWidth 10; MinHeight 10; MaxHeight (root#height - 200)]
(*e: constant Efuns_filebrowser.top *)
(*s: constant Efuns_filebrowser.adx *)
let adx = new WX_adjust.t ()
(*e: constant Efuns_filebrowser.adx *)
(*s: constant Efuns_filebrowser.ady *)
let ady = new WX_adjust.t ()
(*e: constant Efuns_filebrowser.ady *)
(*s: constant Efuns_filebrowser.hbar *)
let hbar = new WX_bar.h top#container []
(*e: constant Efuns_filebrowser.hbar *)
(*s: constant Efuns_filebrowser.viewport *)
let viewport = new WX_viewport.t hbar#container adx ady []
(*e: constant Efuns_filebrowser.viewport *)
(*s: constant Efuns_filebrowser.scrollbar *)
let scrollbar = new WX_scrollbar.v hbar#container ady []
(*e: constant Efuns_filebrowser.scrollbar *)
(*s: constant Efuns_filebrowser.tree *)
let tree = new WX_tree.t viewport#container []
(*e: constant Efuns_filebrowser.tree *)

(*s: function Efuns_filebrowser.load *)
let load filename =
  let _ = Sys.command(Printf.sprintf "efuns_server %s &" filename)
  in ()
(*e: function Efuns_filebrowser.load *)

(*s: function Efuns_filebrowser.iter_load *)
let rec iter_load closed dirname basename container =
  try
    let dirname = Filename.concat dirname basename in
    let filenames = Sort.list (<=) (Utils.list_dir dirname) in
    let subdirs = List.fold_left (fun files filename ->
          if filename <> "." && filename <> ".." then
            let fullname = Filename.concat dirname filename in
            let stats = lstat fullname in
            if stats.st_kind = S_DIR then filename::files else
              files
          else files
      ) [] filenames in
    let file_lists = List.fold_left (fun lists (name,regexp) ->
          let newlist = List.rev (List.fold_left (fun files filename ->
                if Str.string_match regexp filename 0 then filename :: files
                else files
              ) [] filenames) in
          let rec iter pos newlist addlist lists =
            match newlist with
            | ele :: tail -> 
                if pos mod 15 = 0 then
                  iter 1 tail [] (((
                        Printf.sprintf "%s[to %s]" name ele), (ele::addlist)) :: lists)
                else
                  iter (pos+1) tail (ele::addlist) lists
            |   [] -> match addlist with [] -> lists | _ -> 
                    (name,addlist) :: lists
          in
          let newlists = iter 1 newlist [] [] in
          match newlists with
            [] -> lists
          | _ -> (List.rev newlists) @ lists
      ) [] regexps in
    (List.map (fun subdir ->
          let label = new WX_label.t container subdir [] in
          let tree  = new WX_tree.t container [] in
          tree#set_desc (iter_load true dirname subdir tree#container);
          branch true label#contained tree#contained
      ) subdirs) @ (
      List.map (fun (name,files) -> 
          let label = new WX_label.t container name [] in
          let tree  = new WX_tree.t container [] in
          tree#set_desc (List.map (fun filename ->
                let label = new WX_button.with_label tree#container filename 
                  [IpadX 0; IpadY 0] 
                  in
                label#set_action (fun () -> 
                    load (Filename.concat dirname filename));                
                leaf 0 label#contained
            ) (List.rev files));
            branch true label#contained tree#contained         
      ) file_lists
    )
  with
    _ -> []
(*e: function Efuns_filebrowser.iter_load *)

open WX_filesel
  
(*s: constant Efuns_filebrowser.file_menu *)
let file_menu = [|
    "Open", (fun _ -> 
        let info = {
            filter = Filename.concat !dirname  "*";
            current_selection= !dirname;
            predicat = (fun _ -> true);
            action = (fun s -> 
                tree#destroy_desc;
                dirname := s;
                tree#set_desc (
                  iter_load false (Filename.dirname s) (Filename.basename s)
                  tree#container));
            cancel = (fun _ -> ());
          }    in
        let filesel = new WX_filesel.t root info [] in
        filesel#show
        );
    "Quit", (fun _ -> exit 0);
  |]
(*e: constant Efuns_filebrowser.file_menu *)
  
(*s: toplevel Efuns_filebrowser._1 *)
let _ =
  tree#set_desc (iter_load false (Filename.dirname Sys.argv.(1))
    (Filename.basename Sys.argv.(1))
    tree#container);
  top#container_add hbar#contained;
  hbar#container_add_s [viewport#contained; scrollbar#contained];
  viewport#container_add tree#contained;
  top#setWM_NAME (Printf.sprintf "File Browser: %s" Sys.argv.(1));
  top#add_menu "File" file_menu;
  top#show;
  loop ()
(*e: toplevel Efuns_filebrowser._1 *)
(*e: misc/efuns_filebrowser.ml *)
