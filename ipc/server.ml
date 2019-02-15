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
open Common
open Unix
open Efuns
  
(*s: constant [[Server.efuns_property]] *)
(*let efuns_property = "_EFUNS_SERVER"  *)
(*e: constant [[Server.efuns_property]] *)
(*s: constant [[Server.user]] *)
let user = 
  try Sys.getenv "USER" 
  with _ -> "noname"
(*e: constant [[Server.user]] *)
(*s: constant [[Server.socket_name]] *)
let socket_name = 
  (Printf.sprintf "/tmp/efuns-server.%s.%s:0" user !Globals.displayname)
(*e: constant [[Server.socket_name]] *)

(*s: constant [[Server.started]] *)
let started = ref false
(*e: constant [[Server.started]] *)
  
(*s: type [[Server.proto]] *)
type command =
  | LoadFile of Common.filename * int (* pos *) * int (* line *) * string
(*e: type [[Server.proto]] *)

 
(*s: function [[Server.read_command]] *)
let read_command fd frame_opt =
  let inc = in_channel_of_descr fd in
  try
    let cmd = input_value inc in
    match cmd with
    | LoadFile (name, pos, line, str) ->
        let window = 
          match frame_opt with
          | Some frame -> frame.frm_window
          | None -> 
            let edt = Globals.editor () in
            match edt.top_windows with
            | [] -> failwith "no top windows"
            | x::_ -> x.top_active_frame.frm_window
        in
        let frame = Frame.load_file window name in
        let (_, text, point) = Frame.buf_text_point frame in
        if pos <> 0 
        then Text.set_position text point pos;
        if line <> 0 
        then Text.goto_line text point line;
        if str <> ""
        (* bugfix: need the begin/end here, otherwise update below in then *)
        then begin                 
          let regexp = Str.regexp_string str in
          Text.search_forward text regexp point |> ignore;
        end;
        Top_window.update_display () 
  with
    _ -> Concur.remove_reader fd 
(*e: function [[Server.read_command]] *)
  
(*s: function [[Server.module_accept]] *)
let module_accept s frame_opt = 
  let fd,_ = accept s in
  Unix.set_close_on_exec fd;
  Concur.add_reader fd (fun _ -> read_command fd frame_opt)
(*e: function [[Server.module_accept]] *)
  
(*s: function [[Server.start]] *)
let start frame_opt =
  if not !started then
  Utils.catchexn "Efuns server:" (fun _ ->
      let s = Unix.socket PF_UNIX SOCK_STREAM 0 in

      if Sys.file_exists socket_name 
      then begin 
        pr2 (spf "socket file %s already exists; cancelling the server"
              socket_name);
        (* alt: unlink here, so if you run multiple efuns, the last one wins*)
      end else begin
        Unix.bind s (ADDR_UNIX socket_name);
        Unix.listen s 254;
        Unix.set_nonblock s;
        Unix.set_close_on_exec s;
        Concur.add_reader s (fun _ -> 
          started := true;
          module_accept s frame_opt
        );
       Hook.add_hook Misc.exit_hooks (fun () -> Unix.unlink socket_name);
      end
  )  
(*e: function [[Server.start]] *)

let server_start frame =
  start (Some frame)
[@@interactive]
  
(*e: ipc/server.ml *)
