(*s: ipc/efuns_client.ml *)
open Server  
open Xtypes
open Unix
open Sys
open Filename
  
(*s: constant Efuns_client.pos *)
let pos = ref 0
(*e: constant Efuns_client.pos *)
(*s: constant Efuns_client.string *)
let string = ref ""
(*e: constant Efuns_client.string *)
(*s: constant Efuns_client.filename *)
let filename = ref None
(*e: constant Efuns_client.filename *)
  
(*s: toplevel Efuns_client._1 *)
let _ = Arg.parse [
    "-pos", Arg.Int (fun i -> pos := i)," <int>: set position";
    "-str", Arg.String (fun s -> string := s)," <string>: search string";
  ] (fun name -> filename := Some name)
  "efuns_server: connect to efuns"
(*e: toplevel Efuns_client._1 *)
  
(*s: constant Efuns_client.pos (client/efuns_client.ml) *)
let pos = !pos    
(*e: constant Efuns_client.pos (client/efuns_client.ml) *)
(*s: constant Efuns_client.filename (client/efuns_client.ml) *)
let filename = match !filename with
    None -> failwith "efuns_server: filename missing"
  | Some s -> s
(*e: constant Efuns_client.filename (client/efuns_client.ml) *)
(*s: constant Efuns_client.string (client/efuns_client.ml) *)
let string = !string
(*e: constant Efuns_client.string (client/efuns_client.ml) *)
    
(*s: constant Efuns_client.filename (client/efuns_client.ml)2 *)
let filename = if is_relative filename then 
    concat (getcwd ()) filename else filename
(*e: constant Efuns_client.filename (client/efuns_client.ml)2 *)
    
(*s: constant Efuns_client.efuns_property *)
let efuns_property = "_EFUNS_SERVER"  
(*e: constant Efuns_client.efuns_property *)

(*s: constant Efuns_client.display *)
let display = Xlib.openDisplay ""
(*e: constant Efuns_client.display *)
(*s: constant Efuns_client.root *)
let root = display.dpy_roots.(0).scr_root
(*e: constant Efuns_client.root *)
(*s: constant Efuns_client.atom *)
let atom = X.internAtom display efuns_property false
(*e: constant Efuns_client.atom *)
(*s: constant Efuns_client.socket_name *)
let socket_name = (Xlib.getWholeProperty display root atom).gp_value
(*e: constant Efuns_client.socket_name *)
(*s: toplevel Efuns_client._2 *)
let _ =  if not (Sys.file_exists socket_name) then raise Not_found
(*e: toplevel Efuns_client._2 *)
let (inc,outc) = open_connection (ADDR_UNIX socket_name)
(*s: toplevel Efuns_client._3 *)
let _ =  
  output_value outc (LoadFile (filename,pos,string)); flush outc
(*e: toplevel Efuns_client._3 *)
  
(*e: ipc/efuns_client.ml *)
