(*s: features/system.ml *)
open Efuns
open Unix
(*open Concur*)
(*open ThreadUnix*)

(*s: function System.open_process *)
let open_process cmd =
  let (in_read, in_write) = pipe() in
  let (out_read, out_write) = pipe() in
  let inchan = in_channel_of_descr in_read in
  let outchan = out_channel_of_descr out_write in
  match fork() with
    0 ->
      if out_read <> stdin then begin
          dup2 out_read stdin; 
          close out_read 
      end;
      if in_write <> Unix.stdout ||  in_write <> Unix.stderr then begin
          if in_write <> Unix.stdout then dup2 in_write stdout;
          if in_write <> Unix.stderr then dup2 in_write stderr; 
          close in_write 
      end;
      List.iter close [in_read;out_write];
      execv "/bin/sh" [| "/bin/sh"; "-c"; cmd |];
      (*      exit 127 *)
  | pid -> 
      Unix.close out_read;
      Unix.close in_write;
      (pid, inchan, outchan)
(*e: function System.open_process *)

(*s: function System.system *)
let system buf_name cmd end_action =
  let (pid,inc,outc) = open_process cmd in
  let text = Text.create "" in
  let curseur = Text.new_point text in
  let buf = Ebuffer.create buf_name None text (Keymap.create ()) in
  (* !!! *)
  buf.buf_sync <- true;

  let ins = Unix.descr_of_in_channel inc in
  let tampon = String.create 1000 in
  let active = ref true in
  let location = Efuns.location () in
  Concur.Thread.add_reader ins (fun () ->
    let pos,str = Text.delete_res text curseur
        (Text.point_to_eof text curseur) in
    let len = input inc tampon 0 1000 in
    Mutex.lock location.loc_mutex;
    if len = 0 then begin
      let pid,status = waitpid [WNOHANG] pid in
      (match status with 
      | WEXITED s -> Text.insert_at_end text 
              (Printf.sprintf "Exited with status %d\n" s); 
            close_in inc;
            close_out outc;
            (try end_action buf s with _ -> ())
      | _ -> Text.insert_at_end text "Broken pipe" 
      );
      Text.set_position text curseur (Text.size text);
      active := false;
      (* redraw screen *)
      Top_window.update_display ();

      Mutex.unlock location.loc_mutex;
      Concur.Thread.remove_reader ins; (* Kill self *)
    end
    else
      Text.insert_at_end text (String.sub tampon 0 len);

    Text.set_position text curseur (Text.size text);
    Text.insert text curseur str;
    buf.buf_modified <- buf.buf_modified +1;

    (* redraw screen *)
    Top_window.update_display ();
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

  (*s: [[System.system()]] set finalizer, to intercept killed frame *)
  buf.buf_finalizers <- (fun () -> 
      (try 
          Unix.kill pid Sys.sigkill;
          let _,_ = waitpid [] pid in
          ()
        with _ -> ());
      (*Concur.Thread.remove_reader ins*) failwith "System.system: TODO 2"
    )
  :: buf.buf_finalizers;
  (*e: [[System.system()]] set finalizer, to intercept killed frame *)

  buf
(*e: function System.system *)

(*s: function System.start_command *)
let start_command buf_name window cmd =
  let buf = system buf_name cmd (fun _buf _status -> ()) in
  let frame = Frame.create window None buf in
  frame
(*e: function System.start_command *)

(*s: constant System.shell_hist *)
let shell_hist = ref []
(*e: constant System.shell_hist *)
(*s: function System.shell_command *)
let shell_command frame =
  Select.select_string frame "Run command:" shell_hist "" (fun cmd -> 
    start_command "*Command*" (Multi_frames.cut_frame frame) cmd |> ignore)
(*e: function System.shell_command *)
  
(*e: features/system.ml *)
