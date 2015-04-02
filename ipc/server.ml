(*s: ipc/server.ml *)
(*s: copyright header *)
(***********************************************************************)
(*                                                                     *)
(*                             ____________                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
(*e: copyright header *)
open Efuns
open Xtypes
open Unix  
open Top_window
  
(*s: constant Server.efuns_property *)
let efuns_property = "_EFUNS_SERVER"  
(*e: constant Server.efuns_property *)
(*s: constant Server.user *)
let user = 
  try Sys.getenv "USER" 
  with _ -> "noname"
(*e: constant Server.user *)
(*s: constant Server.socket_name *)
let socket_name = (Printf.sprintf "/tmp/efuns-server.%s.%s:0" user !displayname)
(*e: constant Server.socket_name *)

(*s: constant Server.started *)
let started = ref false
(*e: constant Server.started *)
  
(*s: type Server.proto *)
type proto =
  LoadFile of string * int * string
(*e: type Server.proto *)
  
(*s: function Server.read_command *)
let read_command fd frame =
  let inc = in_channel_of_descr fd in
  try
    let cmd = input_value inc in
    match cmd with
    | LoadFile (name,pos,str) ->
        let window = frame.frm_window in
        let top_window = Window.top window in
        wrap top_window (fun top_window ->
            let frame = Frame.load_file window name in
            if pos <> 0 then
              let buf = frame.frm_buffer in
              let text = buf.buf_text in
              let point = frame.frm_point in
              try
                if str = "" then raise Not_found else                  
                let regexp = Str.regexp_string str in
                let len = Text.search_forward text regexp point in
                ()
              with
                Not_found -> 
                  Text.set_position text point pos
        ) ()
  with
    _ -> Concur.Thread.remove_reader fd 
(*e: function Server.read_command *)
  
(*s: function Server.module_accept *)
let module_accept s frame = 
  let fd,_ = accept s in
  Unix.set_close_on_exec fd;
  Concur.Thread.add_reader fd (fun _ -> read_command fd frame)
(*e: function Server.module_accept *)
  
(*s: function Server.start *)
let start frame =
  if not !started then
  let top_window = Window.top frame.frm_window in
  Utils.catchexn "Efuns server:" (fun _ ->
      let s = Unix.socket PF_UNIX SOCK_STREAM 0 in
      if Sys.file_exists socket_name then Unix.unlink socket_name;
      Unix.bind s (ADDR_UNIX socket_name);
      Unix.listen s 254;
      Unix.set_nonblock s;
      Unix.set_close_on_exec s;
      let display = top_window.top_root#display  in
        Concur.Thread.add_reader s (fun _ -> 
            started := true;
            module_accept s frame);
      let atom = X.internAtom display efuns_property false in
      X.changeProperty display top_window.top_root#window 
        PropModeReplace atom XA.xa_string 1 socket_name;
  )  
(*e: function Server.start *)
  

(*e: ipc/server.ml *)
