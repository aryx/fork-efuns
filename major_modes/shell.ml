(* Yoann Padioleau
 *
 * Copyright (C) 2015 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

open Efuns
(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* An eshell inspired shell/terminal for efuns.
*)


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let pwd_var = Local.create_string ""

let pwd buf =
  Efuns.get_local buf pwd_var
  
  

let prompt buf =
  spf "%s $ " (pwd buf)


(*****************************************************************************)
(* Colors *)
(*****************************************************************************)

let prompt_color = "coral"

let colorize buf =
  Dircolors.colorize buf;
  Simple.color buf 
    (Str.regexp ("^/.* \\$")) false
      (Text.make_attr (Window.get_color prompt_color) 1 0 false);
  ()

(*****************************************************************************)
(* Builtins *)
(*****************************************************************************)

let display_prompt buf =
  Text.insert_at_end buf.buf_text "\n";
  Text.insert_at_end buf.buf_text (prompt buf);
  colorize buf

let builtin_ls ?(show_dotfiles=false) frame =
  let buf = frame.frm_buffer in
  let dir = pwd buf in
  let files = Utils.file_list dir in
  let files =
    if show_dotfiles
    then files
    else files |> Common.exclude (fun s -> s =~ "^\\.")
  in

  (* similar to Select.complete_filename *)
  let files = files |> List.map (fun file ->
    let path = Filename.concat dir file in
    try 
      let stat = Unix.stat path in
      match stat.Unix.st_kind with
      | Unix.S_DIR -> file ^ "/"
      | _ -> file
    with exn -> 
      pr2 (spf "builtin_ls: exn = %s" (Common.exn_to_s exn));
      file
  )
  in

  (* similar to Select.display_completions *)
  let rec iter list s =
    match list with
    | [] -> s
    | [f] -> Printf.sprintf "%s\n%s" s f
    | f1::f2::tail  ->
      iter tail (Printf.sprintf "%s\n%-40s%s" s f1 f2)
  in
  Text.insert_at_end buf.buf_text (iter files "");
  display_prompt buf

let builtin_cd frame s =
  let buf = frame.frm_buffer in
  let olddir = Efuns.get_local buf pwd_var in
  let newdir =
    if Filename.is_relative s
    then Filename.concat olddir s |> Common.realpath
    else s
  in
  let stat = Unix.stat newdir in
  match stat.Unix.st_kind with
  | Unix.S_DIR -> 
      Efuns.set_local buf pwd_var newdir;
      builtin_ls frame
  | _ -> failwith (spf "%s is not a directory" newdir)

let builtin_v frame s =
  let buf = frame.frm_buffer in
  display_prompt buf;
  Frame.load_file frame.frm_window s |> ignore
  

(*****************************************************************************)
(* Interpreter *)
(*****************************************************************************)

let run_cmd frame cmd =
  let buf = frame.frm_buffer in
  let (pid,inc,outc) = System.open_process cmd in
  let location = Efuns.location () in
  let text = buf.buf_text in
  Text.insert_at_end text "\n";

  let end_action buf _s = 
    display_prompt buf 
  in
  

  Thread.create (fun () ->
    let tampon = String.create 1000 in

    let finished = ref false in
    while not !finished do
      let len = input inc tampon 0 1000 in
      Mutex.lock location.loc_mutex;
      if len = 0 then begin
        let pid, status = Unix.waitpid [Unix.WNOHANG] pid in
        (match status with 
        | Unix.WEXITED s -> 
            Text.insert_at_end text (spf "Exited with status %d" s); 
            close_in inc;
            close_out outc;
            (try end_action buf s with _ -> ())
        | _ -> Text.insert_at_end text "Broken pipe" 
        );
        finished := true;
      end
      else Text.insert_at_end text (String.sub tampon 0 len);

      Mutex.unlock location.loc_mutex;
      (* redraw screen *)
      Top_window.update_display ();
    done
  ) () |> ignore;
  Thread.delay 0.2;
  ()

let interpret frame s =
  (match s with
  | "ls" -> builtin_ls ~show_dotfiles:true frame
  | "f" -> builtin_ls ~show_dotfiles:false frame
  | _ when s =~ "cd[ ]+\\(.*\\)" -> builtin_cd frame (Common.matched1 s)
  | "s" -> builtin_cd frame ".."
  | _ when s =~ "v[ ]+\\(.*\\)" -> builtin_v frame (Common.matched1 s)
  | cmd -> run_cmd frame cmd
  )

  


(*****************************************************************************)
(* Install *)
(*****************************************************************************)

let install buf =
  Efuns.set_local buf pwd_var 
  (* todo: use the dirname of the file in current frame 
     Frame.current_dir?
  *)
    (Efuns.location()).loc_dirname;
  (* !!! *)
  buf.buf_sync <- true;
  display_prompt buf;
  ()

let mode =  Ebuffer.new_major_mode "Shell" [install]
let shell_mode frame = Ebuffer.set_major_mode frame.frm_buffer mode

let eshell buf_name frame =
  let text = Text.create "" in
  let buf = Ebuffer.create buf_name None text (Keymap.create ()) in
  Ebuffer.set_major_mode buf mode;
  Frame.change_buffer frame.frm_window buf.buf_name;
  ()

let key_return frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  Text.with_dup_point text frame.frm_point (fun point ->
    let delta = Text.search_backward text (Str.regexp " \\$ ") point in
    Text.fmove text point delta;
    let s = Text.region text frame.frm_point point in
    interpret frame s
  )

let eshell_num frame =
  let char = Char.chr !Top_window.keypressed in
  let buf_name = spf "*Shell-%c*" char in
  match Ebuffer.find_buffer_opt buf_name with
  | None -> eshell buf_name frame
  | Some buf -> Frame.change_buffer frame.frm_window buf.buf_name

      
  
  

(*****************************************************************************)
(* Setup *)
(*****************************************************************************)

let _ = 
  Efuns.add_start_hook (fun () ->
    Keymap.define_interactive_action "eshell" (eshell "*Shell*");
    Keymap.define_interactive_action "shell" (eshell "*Shell*");
    Keymap.define_interactive_action "eshell_num" eshell_num;

    Keymap.add_major_key mode [(NormalMap, XK.xk_Return)]
      "key_return" key_return;
  )

