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

module FT = File_type

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An Eshell-inspired shell/terminal for Efuns.
 *
 * todo:
 *  - fix scroll_to_end to handle when have overflow lines
 *  - typing a key should go to the prompt (but do via
 *    handle_key_before is tricky, do only for regular keys without
 *    modifiers)
 *  - M-backspace should not delete the prompt ... 
 *  - C-a should not pass the prompt.
 *  - later: have syntax for more complex stuff inspired by scsh? more
 *    regular. I always get confused about order of redirection, 
 *    the lack of nestedness, etc.
 *  - look at Shell.nw?
 *)

(*****************************************************************************)
(* Variables, globals, and constants *)
(*****************************************************************************)

(* pwd of the shell buffer *)
let pwd_var = Store.create_string "Shell.pwd"
let pwd buf =
  Var.get_local buf pwd_var

(* for C-a *)
let prompt_last_pos = Store.create_abstr "Shell.prompt_last_pos"
(* for Up *)
let history = Store.create_abstr "Shell.history"

let prompt_regexp = Str.regexp ("^/.* \\$ ")
let prompt_color = "coral"


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* for builtin_ls *)
let columnize width xs =
  let maxlen = xs |> List.map String.length |> Common2.maximum in
  (* need to account for extra spaces between columns *)
  let maxlen = maxlen + 2 in
  (* but don't need the extra space for the last col so compensate *)
  let width = width + 2 in

  (* ex: width = 80  maxlen = 10 => nbcols = 8. maxlen = 11 => nbcols = 7 *)
  let nbcols = width / maxlen in
  (* ex xs len = 16 nbcols = 8 => nblines = 2. nbcols = 7 => nblines = 3 *) 
  let nblines = 
    List.length xs / nbcols +
    (if List.length xs mod nbcols = 0 then 0 else 1)
  in
  let space_per_col = width / nbcols in
  let arr = Array.of_list xs in
  let buf = Buffer.create (nblines * width) in
  for i = 0 to nblines - 1 do
    for j = 0 to nbcols - 1 do
      let idx = i + j * nblines in
      let s =
        try arr.(idx)
        with Invalid_argument 
            (* it's Array.get in ocaml light :) *)
            ("index out of bounds" | "Array.get") -> ""
      in
      let len = String.length s in
      Buffer.add_string buf s;
      let nbspaces = space_per_col - len in
      if j <> nbcols - 1
      then Buffer.add_string buf (String.make nbspaces ' ');
    done;
    if i <> nblines - 1 
    then Buffer.add_string buf "\n";
  done;
  Buffer.contents buf

(* to filter them in ls *)
let is_obj_file file =
  try
    let typ = FT.file_type_of_file file in
    match typ with
    | FT.Obj _ -> true
    | _ -> false
  with _ -> false

(* plan9: ugly patching for plan. Still needed? *)
let vfat_patch x =
  if x =~ "^[A-Z0-9_]*\\.[A-Z0-9_]+$" ||
     x =~ "^[A-Z0-9_]+$"
  then String.lowercase x
  else x

(*****************************************************************************)
(* Scrolling *)
(*****************************************************************************)

(* for Eshell we don't want to have the cursor centered in the frame,
 * we want the cursor at the end so we can see as much as possible
 * output from previous command.
 * later: do like in rc/eshell and scroll until can
 *)

(* assumes have done a Simple.end_of_file frame before *)
let scroll_to_end frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in

  let height = frame.frm_height - frame.frm_has_status_line in
  let point = frame.frm_point in
  let start = frame.frm_start in

  frame.frm_redraw <- true;
  frame.frm_force_start <- true;
  (* the problem is that when goes back to the shell buffer then
   * the saved frm_start with be the last line, not the last line
   * - height + 2.
   * todo: actually compute the right frm_start instead of playing
   * with frm_y_offset?
   *)
  Text.goto_point text start point;
  (* if put just +1 then the cursor goes to a weird place *)
  frame.frm_y_offset <- - height + 2;
  ()

let scroll_until_not_pass_prompt frame =
  let _height = frame.frm_height in
  let _y = frame.frm_y_offset in
  (*pr2_gen (height, y); *)
  ()

(*****************************************************************************)
(* Colors *)
(*****************************************************************************)

let colorize buf =
  (* actually pretty good! *)
  Dircolors.colorize buf;
  (* do that after Dircolors otherwise part of prompt get blue *)
  Color.color buf prompt_regexp false
      (Text.make_attr (Attr.get_color prompt_color) 1 0 false);
  ()

(*****************************************************************************)
(* Prompt *)
(*****************************************************************************)

let prompt buf =
  spf "%s $ " (pwd buf)

(* assumes other commands don't output their final newline *)
let display_prompt buf =
  let text = buf.buf_text in
  Text.insert_at_end text "\n";
  Text.insert_at_end text (prompt buf);
  let last_pos = Var.get_local buf prompt_last_pos in
  Text.set_position text last_pos (Text.size text);
  (* less: it recolorize everything, so might be expensive if we don't
   * limit the nblines of the buffer. A colorize_region would be better.
   *)
  colorize buf

(*****************************************************************************)
(* Builtins *)
(*****************************************************************************)

let builtin_ls (*?(show_dotfiles=false) ?(show_objfiles=false)*)
 show_dotfiles show_objfiles frame =
  let buf = frame.frm_buffer in
  let dir = pwd buf in
  let files = 
    Utils.file_list dir 
    |> List.map vfat_patch
    |> List.sort (fun a b ->
    compare a b (* (String.lowercase a) (String.lowercase b)*)
  )
  in
  let files =
    if show_dotfiles
    then files
    else files |> Common.exclude (fun s -> s =~ "^\\.")
  in
  let files =
    if show_objfiles
    then files
    else files |> Common.exclude is_obj_file
  in

  (* similar to Select.complete_filename *)
  let files = files |> List.map (fun file ->
    let path = Filename.concat dir file in
    try 
      let stat = Unix.stat path in
      match stat.Unix.st_kind with
      (* eshell does not do that, but my Dircolors.colorize needs that *)
      | Unix.S_DIR -> file ^ "/"
      | _ -> file
    with exn -> 
      pr2 (spf "builtin_ls: exn = %s" (Common.exn_to_s exn));
      file
  )
  in
  let s = columnize frame.frm_width files in
  Text.insert_at_end buf.buf_text s;
  display_prompt buf

let builtin_l frame =
  let buf = frame.frm_buffer in
  let dir = pwd buf in
  let files = Utils.file_list dir in

  files |> List.iter (fun file ->
    let path = Filename.concat dir file in
    try 
      let stat = Unix.stat path in
      Text.insert_at_end buf.buf_text 
        (spf "%20d %s\n" 
           stat.Unix.st_size
           file);

    with exn -> 
      pr2 (spf "builtin_ls: exn = %s" (Common.exn_to_s exn))
  );
  display_prompt buf


(* later: handle cd - *)
let builtin_cd frame s =
  let buf = frame.frm_buffer in
  let olddir = Var.get_local buf pwd_var in
  let newdir =
    if Filename.is_relative s
    then Filename.concat olddir s |> Common.fullpath
    else s
  in
  let stat = Unix.stat newdir in
  match stat.Unix.st_kind with
  | Unix.S_DIR -> 
      Var.set_local buf pwd_var newdir;
      (* so C-x C-f and other stuff starts from this directory *)
      let edt = Globals.editor () in
      edt.edt_dirname <- newdir;
      builtin_ls false false frame
  | _ -> failwith (spf "%s is not a directory" newdir)

let builtin_v frame s =
  let (buf, text, _) = Frame.buf_text_point frame in

  let dir = pwd buf in
  let file = 
    if Filename.is_relative s
    then Filename.concat dir s
    else s
  in
  Text.insert_at_end text (spf "Visiting %s" s);
  display_prompt buf;

  Multi_buffers.set_previous_frame frame;
  Frame.load_file frame.frm_window file |> ignore
  

(*****************************************************************************)
(* Interpreter *)
(*****************************************************************************)

let pid_external = ref None

let kill_external frame =
  let (buf, text, _) = Frame.buf_text_point frame in
  match !pid_external with
  | None ->  
      failwith "No external process to kill"
  | Some pid ->
      Unix.kill pid 9;
      Text.insert_at_end text (spf "Killed %d, signal 9" pid);
      display_prompt buf
      
  

let run_cmd frame cmd =
  let (buf, text, _) = Frame.buf_text_point frame in

  let (pid,inc,outc) = System.open_process (pwd buf) cmd in
  pid_external := Some pid;
  let edt = Globals.editor () in

  let end_action buf _s = 
    display_prompt buf 
  in
  
  Thread.create (fun () ->
    let tampon = String.create 1000 in

    let finished = ref false in
    while not !finished do
      let len = input inc tampon 0 1000 in
      Mutex.lock edt.edt_mutex;
      if len = 0 then begin
        let pid, status = Unix.waitpid [Unix.WNOHANG] pid in
        (match status with 
        | Unix.WEXITED s -> 
            Text.insert_at_end text (spf "Exited with status %d" s); 
            close_in inc;
            close_out outc;
            pid_external := None;
            (try end_action buf s with _ -> ())
        | _ -> 
          Text.insert_at_end text "Broken pipe";
          display_prompt buf
        );
        finished := true;
      end
      else Text.insert_at_end text (String.sub tampon 0 len);

      Mutex.unlock edt.edt_mutex;
      scroll_until_not_pass_prompt frame;
      (* redraw screen *)
      Top_window.update_display ();
    done
  ) () |> ignore

let interpret frame s =
 try 
  (* very rudimentary parsing, hmm *)
  (match s with

  (* dir listing *)
  | "ls" -> builtin_ls (*~show_dotfiles:*)true (*~show_objfiles:*)true frame
  | "f" -> builtin_ls (*~show_dotfiles:*)false (*~show_objfiles:*)false frame
  | "l" -> builtin_l frame

  (* dir navig *)
  | "s" -> builtin_cd frame ".."
  | "cd" -> builtin_cd frame Utils.homedir
  | _ when s =~ "cd[ ]+\\(.*\\)" -> builtin_cd frame (Common.matched1 s)

  (* file editing *)
  | _ when s =~ "[ve][ ]+\\(.*\\)" -> 
    builtin_v frame (Common.matched1 s);
    Move.end_of_file frame;
  (* do not scroll_to_end() here! Indeed this will change frm_start
   * and frm_y_offset, but only frm_start is saved in the
   * buffer, and so a restore to the eshell buffer will have a wrong
   * frm_start.
   *)
    raise Exit

  (* general case *)
  | cmd -> run_cmd frame cmd
  );
  Move.end_of_file frame;
  scroll_to_end frame;
  ()
 with Exit -> ()

(*****************************************************************************)
(* Keys and actions *)
(*****************************************************************************)

(* initiate the interpreter *)
let key_return frame =
  let (buf, text, point) = Frame.buf_text_point frame in

  Text.insert_at_end text "\n";
  Text.with_dup_point text point (fun cursor ->
    let delta = Text.search_backward text prompt_regexp cursor in
    Text.fmove text cursor delta;
    let s = Text.region text point cursor in
    interpret frame s
  )

(* helper *)
let if_else_after_last_prompt fthen felse frame =
  let (buf, text, point) = Frame.buf_text_point frame in
  let last_pos = Var.get_local buf prompt_last_pos in
  if point > last_pos
  then fthen frame
  else felse frame

let goto_prompt frame =
  let (buf, text, point) = Frame.buf_text_point frame in
  let last_pos = Var.get_local buf prompt_last_pos in
  Text.goto_point text point last_pos
[@@interactive]

(*****************************************************************************)
(* Install *)
(*****************************************************************************)

let install buf =
  let text = buf.buf_text in
  (* loc_dirname should be the dirname of the file in active frame *)
  Var.set_local buf pwd_var  (Globals.editor()).edt_dirname;
  Var.set_local buf prompt_last_pos (Text.new_point text);
  Var.set_local buf history (ref []);

  let tbl = Ebuffer.create_syntax_table () in
  buf.buf_syntax_table <- tbl;
  tbl.(Char.code '_') <- true;
  tbl.(Char.code '-') <- true;
  tbl.(Char.code '.') <- true;

  display_prompt buf;
  Text.set_position text buf.buf_point (Text.size text);
  ()

let mode =  Ebuffer.new_major_mode "Shell" (Some install)

let eshell buf_name frame =
  let text = Text.create "" in
  let buf = Ebuffer.create buf_name None text (Keymap.create ()) in
  Ebuffer.set_major_mode buf mode;
  Multi_buffers.set_previous_frame frame;
  Frame.change_buffer frame.frm_window buf.buf_name;
  ()

let shell =
  eshell "*Shell*"
[@@interactive]

(* use Top_window.keypressed *)
let eshell_num frame =
  let char = Char.chr !Top_window.keypressed in
  let buf_name = spf "*Shell-%c*" char in
  match Ebuffer.find_buffer_opt buf_name with
  | None -> eshell buf_name frame
  | Some buf -> 
      Multi_buffers.set_previous_frame frame;
      Frame.change_buffer frame.frm_window buf.buf_name
[@@interactive]

(*****************************************************************************)
(* Setup *)
(*****************************************************************************)

let _ = 
  Hook.add_start_hook (fun () ->
       (* major mode keys *)
   [
    (* pretty good completion for free *)
    [(NormalMap, XK.xk_Tab)], Abbrevs.dabbrev_expand;
   
    [Keymap.c_c; (ControlMap, Char.code 'k')], kill_external;

    (* patching traditional keys *)
    [(NormalMap, XK.xk_Return)], key_return;
    [(MetaMap, Char.code '>')], (fun frame ->
        Move.end_of_file frame;
        scroll_to_end frame;
      );
    [ControlMap, Char.code 'a'], if_else_after_last_prompt 
      goto_prompt Move.beginning_of_line;
    [MetaMap, XK.xk_BackSpace ], if_else_after_last_prompt 
        (Frame.to_frame Edit.delete_backward_word)
        (fun _ -> ());

    (* less: more patching?
     [NormalMap, XK.xk_Left], (fun frm -> ignore (Move.move_backward frm 1)); 
     [ControlMap, XK.xk_Left ], (Frame.to_frame Move.backward_word);
     [ControlMap, Char.code 'r'], Search.isearch_backward;
     *)
   ] |> List.iter (fun (key, action) -> Keymap.add_major_key mode key action);

(* buggy, does not handle C-e, need something better
    Efuns.set_major_var mode Top_window.handle_key_start_hook [(fun frame ->
      let keysym = !Top_window.keypressed in
      if Char.code 'A' <= keysym && keysym <= Char.code 'z'
      then Simple.end_of_file frame;
      ()
    )];
*)

  )
