open Server  
open Xtypes
open Unix
open Sys
open Filename
  
let pos = ref 0
let string = ref ""
let filename = ref None
  
let _ = Arg.parse [
    "-pos", Arg.Int (fun i -> pos := i)," <int>: set position";
    "-str", Arg.String (fun s -> string := s)," <string>: search string";
  ] (fun name -> filename := Some name)
  "efuns_server: connect to efuns"
  
let pos = !pos    
let filename = match !filename with
    None -> failwith "efuns_server: filename missing"
  | Some s -> s
let string = !string
    
let filename = if is_relative filename then 
    concat (getcwd ()) filename else filename
    
let efuns_property = "_EFUNS_SERVER"  

let display = Xlib.openDisplay ""
let root = display.dpy_roots.(0).scr_root
let atom = X.internAtom display efuns_property false
let socket_name = (Xlib.getWholeProperty display root atom).gp_value
let _ =  if not (Sys.file_exists socket_name) then raise Not_found
let (inc,outc) = open_connection (ADDR_UNIX socket_name)
let _ =  
  output_value outc (LoadFile (filename,pos,string)); flush outc
  