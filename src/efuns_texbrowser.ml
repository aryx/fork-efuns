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


let display = new WX_display.t ""
let root = new WX_root.t display 0
let top = new WX_appli.t root [MinWidth 10; MinHeight 10]
let tree = new WX_tree.t top#container []

let regexp = Str.regexp "\(\\input\|\\section\|\\subsection\|\\subsubsection\|\\chapter\)[*]?{\([^}]+\)}"

let load filename pos all =
  let _ = Sys.command(Printf.sprintf "efuns_server %s -pos %d -str \"%s\" &" filename pos all)
  in ()

let filename = ref (if Array.length Sys.argv = 2 then
      Sys.argv.(1) else 
      failwith "Usage: efuns_texbrowser filename")
  
let dirname = ref (Filename.dirname !filename)
let path = ref [ !dirname ]
  
let rec iter_load closed filename container =
  let label = new WX_button.with_label container filename [] in
  try
    let filename = try
        Utils.find_in_path !path filename
      with Not_found -> 
          Utils.find_in_path !path (filename^".tex")
    in
    label#set_action (fun () -> load filename 0 "");
    let inc = open_in filename in
    let s = Utils.read_string inc in
    close_in inc;
    let rec iter_search pos list = 
      try
        let newpos = Str.search_forward regexp s pos in
        let keyword = Str.matched_group 1 s in
        let all = Str.matched_group 0 s in
        iter_search (newpos+String.length keyword) (
          (all, keyword,Str.matched_group 2 s,newpos)::list)
      with
        _ -> List.rev list
    in
    let list =  iter_search 0 [] in
    if list = [] then leaf 0 label#contained else
    let tree2 = new WX_tree.t container [] in
    tree2#set_desc (List.map (
        fun (all,keyword,name,pos) -> 
          match keyword with
            "input" -> iter_load true name tree2#container
          | _ ->
              let (offset,prefix) =
                match keyword with
                  "section" -> 0, "s:"
                | "subsection" -> 10, "ss:"
                | "subsubsection" -> 20, "sss:"
                | _ -> 30,""
              in
              leaf offset (
                  let button = new WX_button.with_label tree2#container (
                      prefix^name) [] 
                  in
                  button#set_action (fun () -> load filename pos all);
                  button#contained
                  )
      ) list);
    branch closed label#contained tree2#contained
  with
    _ -> 
      leaf 0 label#contained

open WX_filesel
  
let file_menu = [|
    "Open", (fun _ -> 
        let info = {
            filter = Filename.concat !dirname  "*.tex";
            current_selection= !dirname;
            predicat = (fun _ -> true);
            action = (fun s -> 
                tree#destroy_desc;
                filename := s;
                dirname := Filename.dirname s;
                top#setWM_NAME (Printf.sprintf "LaTeX Browser: %s" !filename);
                tree#set_desc [iter_load false s tree#container]);
            cancel = (fun _ -> ());
          }    in
        let filesel = new WX_filesel.t root info [] in
        filesel#show              
        );
    "Quit", (fun _ -> exit 0);
  |]

let _ =
  tree#set_desc [iter_load false Sys.argv.(1) tree#container];
  top#setWM_NAME (Printf.sprintf "LaTeX Browser: %s" !filename);
  top#container_add tree#contained;
  top#add_menu "File" file_menu;
  top#add_button "Reload" (fun _ () ->
      tree#destroy_desc;
      tree#set_desc [iter_load false !filename tree#container];      
  );
  top#add_separator;
  top#show;
  loop ()