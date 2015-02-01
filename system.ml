open Efuns
open Unix
open Concur
open ThreadUnix
open Select
open Simple
open Top_window
open Multi_frames

let open_process cmd =
  let (in_read, in_write) = pipe() in
  let (out_read, out_write) = pipe() in
  let inchan = in_channel_of_descr in_read in
  let outchan = out_channel_of_descr out_write in
  match fork() with
    0 ->
      if out_read <> stdin then begin
          dup2 out_read stdin; close out_read end;
      if in_write <> Unix.stdout ||  in_write <> Unix.stderr then begin
          if in_write <> Unix.stdout then dup2 in_write stdout;
          if in_write <> Unix.stderr then dup2 in_write stderr; 
          close in_write end;
      List.iter close [in_read;out_write];
      execv "/bin/sh" [| "/bin/sh"; "-c"; cmd |];
      exit 127
  | pid -> 
      Unix.close out_read;
      Unix.close in_write;
      (pid,inchan, outchan)

let system buf_name location cmd end_action =
  let (pid,inc,outc) = open_process cmd in
  let text = Text.create "" in
  let curseur = Text.add_point text in
  let buf = Ebuffer.create location buf_name None text (Keymap.create ()) in
  buf.buf_sync <- true;
  let ins = Unix.descr_of_in_channel inc in
  let tampon = String.create 1000 in
  let active = ref true in
  Thread.add_reader ins
    (function () ->
        let pos,str = Text.delete_res text curseur
            (Text.point_to_eof text curseur) in
        let len = input inc tampon 0 1000 in
        Mutex.lock location.loc_mutex;
        if len = 0 then
          let pid,status = waitpid [WNOHANG] pid in
          begin
            match status with 
              WEXITED s -> Text.insert_at_end text 
                  (Printf.sprintf "Exited with status %d\n" s); 
                close_in inc;
                close_out outc;
                (try end_action buf s with _ -> ())
            | _ -> Text.insert_at_end text "Broken pipe" 
          end;
          Text.set_position text curseur (Text.size text);
          active := false;
          (* redraw screen *)
          update_display location;
          WX_xterm.update_displays ();
          Mutex.unlock location.loc_mutex;
          Thread.remove_reader ins; (* Kill self *)
        else
          Text.insert_at_end text (String.sub tampon 0 len);
        Text.set_position text curseur (Text.size text);
        Text.insert text curseur str;
        buf.buf_modified <- buf.buf_modified +1;
        (* redraw screen *)
        update_display location;
        WX_xterm.update_displays ();
        Mutex.unlock location.loc_mutex
  );
  let lmap = buf.buf_map in
  Keymap.add_binding lmap [NormalMap, XK.xk_Return]
    (fun frame ->
      let point = frame.frm_point in
      Text.insert text point "\n";
      Text.fmove text point 1;
      if !active then (* to avoid a segmentation fault in Ocaml *)
        let str = Text.sub text curseur 
            (Text.point_to_eof text curseur) in
        Text.set_position text curseur (Text.size text);
          (* synchronize viewpoint *)
        output outc str 0 (String.length str);
        flush outc
  );
  buf.buf_finalizers <- 
    (fun () -> 
      (try 
          Unix.kill pid Sys.sigkill;
          let _,_ = waitpid [] pid in ()
        with _ -> ());
      Concur.Thread.remove_reader ins)
  :: buf.buf_finalizers;
  buf

let start_command buf_name window cmd =
  let top_window = Window.top window in
  let location = top_window.top_location in
  let buf = system buf_name location cmd (fun buf status -> ()) in
  let frame = Frame.create window None buf in
  frame

let shell_hist = ref []
let shell_command frame =
  select_string frame "Run command:" shell_hist ""
    (fun cmd -> let _ = start_command "*Command*" (cut_frame frame) cmd in ())
  