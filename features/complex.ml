(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Unix
open Utils
open Efuns
open Text
open Frame
open Simple
open Select
open Interactive

let rec save_buffers_and_action frame buffers action =
  match buffers with
    [] -> let () = action frame in ()
  | (_,buf) :: buffers ->
      let text = buf.buf_text in
      if buf.buf_last_saved = version text  ||
        buf.buf_name.[0] = '*'
      then
        save_buffers_and_action frame buffers action
      else
      let map = Keymap.create () in
      let request = Printf.sprintf "Save buffer %s ? (y,n,!,a)" buf.buf_name
      in
      let yes mini_frame =
        Minibuffer.kill mini_frame frame;
        Ebuffer.save buf;
        save_buffers_and_action frame buffers action
      in
      let no mini_frame =
        Minibuffer.kill mini_frame frame;
        save_buffers_and_action frame buffers action; ()
      in
      let action_immediately mini_frame = 
        Minibuffer.kill mini_frame frame;
        let () = action mini_frame in ()
      in
      let abort mini_frame =
        Minibuffer.kill mini_frame frame
      in
      Keymap.add_binding map [NormalMap, Char.code 'y'] yes;
      Keymap.add_binding map [NormalMap, Char.code 'Y'] yes;
      Keymap.add_binding map [NormalMap, Char.code 'n'] no;
      Keymap.add_binding map [NormalMap, Char.code 'N'] no;
      Keymap.add_binding map [NormalMap, Char.code '!'] action_immediately;
      Keymap.add_binding map [NormalMap, Char.code 'a'] abort;
      Keymap.add_binding map [NormalMap, Char.code 'A'] abort;
      Keymap.add_binding map [ControlMap, Char.code 'g'] abort;
      let _ = Minibuffer.create frame map request in ()

let buf_mtime = Local.create "buf_mtime" string_of_float float_of_string

let update_time buf =
  try
    match buf.buf_filename with
      None -> ()
    | Some file ->
        let st = Unix.lstat file in
        if st.st_kind = S_REG then begin
            set_local buf buf_mtime st.st_mtime;
          end
  with _ -> ()
      
let reload frame = 
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  match buf.buf_filename with
    None -> ()
  | Some file ->
      let inc = open_in file in
      let s = read_string inc in
      close_in inc;
      let point = frame.frm_point in
      let pos = Text.get_position text point in
      Text.clear buf.buf_text;
      Text.insert_at_end text s;
      Text.set_position text point pos;
      List.iter (fun f -> f buf) buf.buf_major_mode.maj_hooks;
      List.iter (fun minor_mode -> List.iter
          (fun f -> f buf) minor_mode.min_hooks) buf.buf_minor_modes;
      Frame.status_modified frame false 
      
let check_file frame =
  try
    let buf = frame.frm_buffer in
    match buf.buf_filename with
      None -> ()
    | Some file ->
        let st = Unix.lstat file in
        if st.st_kind = S_REG then
          try
            let time = get_local buf buf_mtime in
            set_local buf buf_mtime st.st_mtime;
            if time <> st.st_mtime then
              ignore (select_yes_or_no frame 
                  (Printf.sprintf 
                    "%s changed on disk; reload (y/n) ?" 
                    buf.buf_name) (fun bool ->
                    if bool then reload frame else
                      Frame.status_modified frame true
                      ))
          with _ -> 
              set_local buf buf_mtime st.st_mtime
  with _ -> ()
    
let exit_efuns frame =
  let top_window = Window.top frame.frm_window in
  let location = top_window.top_location in
  let buffers = list_of_hash location.loc_buffers in
  save_buffers_and_action frame buffers (fun _ -> exit 0)

let save_some_buffers frame =
  let top_window = Window.top frame.frm_window in
  let location = top_window.top_location in
  let buffers = list_of_hash location.loc_buffers in
  save_buffers_and_action frame buffers (fun _ -> ())

let load_buffer frame = 
  set_previous_frame frame;
  select_filename frame "Find file: " 
    (fun str -> 
      let _ = Frame.load_file frame.frm_window str in ())

let insert_file frame =
  select_filename frame "Insert file: "
    (fun str ->
      let inc = open_in str in
      insert_string frame (Utils.read_string inc);
      close_in inc
  )

let write_buffer frame = 
  let buf = frame.frm_buffer in
  select_filename frame "Save file as: " 
    (fun str -> 
      let top_window = Window.top frame.frm_window in
      Ebuffer.change_name top_window.top_location buf str;
      Ebuffer.save buf)

let save_buffer frame =
  let buf = frame.frm_buffer in
  match buf.buf_filename with
    Some _ -> Ebuffer.save buf
  | None -> write_buffer frame

let window_load_buffer frame = 
  select_filename frame "Find file: " 
    (fun str -> 
      let top_window = Window.top frame.frm_window in
      let top_window = Top_window.create top_window.top_location
          (Window.display top_window)
      in
      let _ = Frame.load_file top_window.top_windows str in ())

let change_buffer frame =
  let default = get_previous_frame () in
  set_previous_frame frame;
  select_buffer frame " Switch to buffer: " default (fun str ->
      let window = frame.frm_window in
      change_buffer window str)

let window_change_buffer frame =
  select_buffer frame "Switch to buffer in new frame: " 
    (get_previous_frame ())
  (fun name ->
      let top_window = Window.top frame.frm_window in
      let top_window = Top_window.create 
          top_window.top_location (Window.display top_window) in
      Frame.change_buffer top_window.top_windows name
  )

let change_font frame =
  let _ = Minibuffer.create_return 
    frame (Keymap.create ()) "Find font: " "fixed"
    (fun old_frame name ->
      let window = frame.frm_window in
      let top_window = Window.top window in
      let xterm = Window.xterm top_window in
      WX_xterm.change_font xterm name
  ) in ()

let color buf regexp strict attr =
  let text = buf.buf_text in
  let point = Text.add_point text in
  try
    while true do
      let len = Text.search_forward text regexp point in
      let before =
        if Text.bmove_res text point 1 = 1 then
          (let c = Text.get_char text point in
            Text.fmove text point (len+1);c)
        else
          (let c = Text.get_char text point in
            Text.fmove text point (len+1); c)
      in
      let after = Text.get_char text point in
      if not (strict && (buf.buf_syntax_table.(Char.code before) ||
            buf.buf_syntax_table.(Char.code after))) then
        begin
          bmove text point len;
          Text.set_attr text point len attr;
          fmove text point len;
          ()
        end
    done
  with
    Not_found -> 
      Text.remove_point text point;
      buf.buf_modified <- buf.buf_modified + 1

let display_hist = ref []
let open_display frame =
  select frame "open_display :" display_hist ""
    (fun _ -> [])
  (fun s -> s)
  (fun name -> 
      let top_window = Window.top frame.frm_window in
      let location = top_window.top_location in
      let dpy_oo = new WX_display.t name in
      let root_oo = new WX_root.t dpy_oo 0 in
      let display = WX_xterm.create_display root_oo
          location.loc_colors_names location.loc_fonts_names
        in
      let _ = Top_window.create location display in
      ())

let goto_line frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  simple_select frame "goto-line:" 
    (fun name ->
      let line = int_of_string name in
      Text.goto_line text point (line - 1))

let goto_char frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  simple_select frame "goto-char:" 
    (fun name ->
      let char = int_of_string name in
      Text.set_position text point char)


let get_pos frame =
  let buf = frame.frm_buffer in
  let point = frame.frm_point in
  let text = buf.buf_text in
  let top_window = Window.top frame.frm_window in
  Top_window.message top_window 
    (Printf.sprintf "Char position %d" 
      (get_position text point))

let mark_at_point frame =
  let top_window = Window.top frame.frm_window in
  let buf = frame.frm_buffer in
  let point = frame.frm_point in
  Ebuffer.set_mark buf point;
  Top_window.message top_window "Mark set";
  ()

let umask = let old = Unix.umask 0 in 
  let _ = Unix.umask old in old
  
let file_perm = Local.create "file_perm" string_of_int int_of_string
let mkdir frame =
  select_filename frame "Make directory: "
    (fun str -> 
      let file_perm = try get_var frame.frm_buffer file_perm with _ -> 
            0x1ff land (lnot umask) in
      Unix.mkdir str file_perm)

let eval_history = ref []
let eval frame =
  select_string frame "Eval:" eval_history "" 
    (fun str ->
      let top_window = Window.top frame.frm_window in
      (* This is not enough: the paths also may have changed. *)
      Top_window.message top_window (Dyneval.eval 
          (let len = String.length str in
          if str.[len - 1] = ';' && str.[len -2 ] = ';' then str else
          str ^ " ;;")))

let variable_hist = ref []
let value_hist = ref []
  
let all_vars = ref None
let all_variables frame _ =
  let buf = frame.frm_buffer in
  let location = buf.buf_location in
  match !all_vars with
    Some (f,l) when f == frame -> l
  | _ ->
      let list = (Local.list buf.buf_vars) @ (Local.list location.loc_vars) in
      all_vars := Some (frame, list);
      list
  
let set_local_variable frame = 
  Select.select frame "set-local-variable : " variable_hist
    "" (all_variables frame) (fun s -> s) (fun variable ->
      Select.select_string frame (Printf.sprintf "%s : " variable)
      value_hist "" (fun value ->
          Local.input frame.frm_buffer.buf_vars variable value))
  
let set_global_variable frame =
  Select.select frame "set-global-variable : " variable_hist
    "" (all_variables frame) (fun s -> s) (fun variable ->
      Select.select_string frame (Printf.sprintf "%s : " variable)
      value_hist "" (fun value ->
          Local.input frame.frm_location.loc_vars variable value))
  
let get_variable frame = 
  Select.select frame "get-variable : " variable_hist
    "" (all_variables frame) (fun s -> s) (fun variable ->
      Top_window.mini_message frame 
        (Printf.sprintf "%s : %s" variable (
          let buf = frame.frm_buffer in
          try
            Local.print buf.buf_vars variable
          with _ ->
              Local.print buf.buf_location.loc_vars variable)))

open Options
  
let parameters_hist = ref []
  
let set_parameter frame = 
  let parameters = get_global frame.frm_location parameters_var in
  Select.select frame "set-parameter : " parameters_hist
    "" (all_parameters frame) (fun s -> s) (fun variable ->
      Select.select_string frame (Printf.sprintf "%s : " variable)
      value_hist "" (fun value ->
          let (input,print,param) = List.assoc variable parameters
          in
          param =:= input value))
  
let get_parameter frame =
  let parameters = get_global frame.frm_location parameters_var in  
  Select.select frame "get-parameter : " parameters_hist
    "" (all_parameters frame) (fun s -> s) (fun variable ->
      Top_window.mini_message frame 
        (Printf.sprintf "%s : %s" variable (
          let (input,print,param) = List.assoc variable parameters
          in
          print !!param)))

let up_buffer = ref ""
  
let down_buffer frame = up_buffer := frame.frm_buffer.buf_name
let up_buffer frame =
  if !up_buffer = "" then raise Not_found;
  set_previous_frame frame;
  Frame.change_buffer frame.frm_window !up_buffer
  
let left_buffer frame =
  set_previous_frame frame;
  Frame.change_buffer frame.frm_window (
    match !prev_buffers with
      name :: buffer :: tail ->
        prev_buffers := tail @ [name]; 
        buffer
    | _ -> raise Not_found)

let right_buffer frame =
  set_previous_frame frame;
  Frame.change_buffer frame.frm_window (match !prev_buffers with
      name :: tail ->
        begin
          match List.rev tail with
            buffer :: tail ->
              prev_buffers := name :: (List.rev tail);
              buffer
          | _ -> raise Not_found
        end
    | _ -> raise Not_found)
  
  
let _ =
  Efuns.add_start_hook (fun location ->
      Keymap.add_interactive location.loc_map "make-directory" mkdir;
      Keymap.add_interactive location.loc_map "set-local-variable" 
        set_local_variable;
      Keymap.add_interactive location.loc_map "set-global-variable" 
        set_global_variable;
      Keymap.add_interactive location.loc_map "get-variable" get_variable;
      Keymap.add_interactive location.loc_map "set-parameter" set_parameter;
      Keymap.add_interactive location.loc_map "get-parameter" get_parameter;
      
  )
  