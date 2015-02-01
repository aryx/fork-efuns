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

open Efuns
open Xtypes
open Unix  
open Top_window
  
let efuns_property = "_EFUNS_SERVER"  
let user = try Sys.getenv "USER" with _ -> "noname"
let socket_name = (Printf.sprintf "/tmp/efuns-server.%s.%s:0" user !displayname)

let started = ref false
  
type proto =
  LoadFile of string * int * string
  
let read_command fd frame =
  let inc = in_channel_of_descr fd in
  try
    let cmd = input_value inc in
    match cmd with
      LoadFile (name,pos,str) ->
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
  
let module_accept s frame = 
  let fd,_ = accept s in
  Unix.set_close_on_exec fd;
  Concur.Thread.add_reader fd (fun _ -> read_command fd frame)

  
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
  

