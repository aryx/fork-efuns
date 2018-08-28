(*s: features/complexe.ml *)
(*s: copyright header2 *)
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
(*e: copyright header2 *)
open Efuns
open Unix

(*s: function [[Complex.save_buffers_and_action]] *)
let rec save_buffers_and_action frame buffers action =
  match buffers with
    [] -> let () = action frame in ()
  | (_,buf) :: buffers ->
      let text = buf.buf_text in
      if buf.buf_last_saved = Text.version text  ||
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
      Minibuffer.create frame map request |> ignore
(*e: function [[Complex.save_buffers_and_action]] *)

(*s: constant [[Complex.buf_mtime]] *)
let buf_mtime = Store.create "buf_mtime" string_of_float float_of_string
(*e: constant [[Complex.buf_mtime]] *)

(*s: function [[Complex.update_time]] *)
let update_time buf =
  try
    buf.buf_filename |> Common.do_option (fun file ->
      let st = Unix.lstat file in
      if st.st_kind = S_REG 
      then Var.set_local buf buf_mtime st.st_mtime;
    )
  with _ -> ()
(*e: function [[Complex.update_time]] *)
      
(*s: function [[Complex.reload]] *)
let reload frame = 
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  match buf.buf_filename with
    None -> ()
  | Some file ->
      let inc = open_in file in
      let s = Utils.read_string inc in
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
(*e: function [[Complex.reload]] *)
      
(*s: function [[Complex.check_file]] *)
let check_file frame =
  try
    let buf = frame.frm_buffer in
    buf.buf_filename |> Common.do_option (fun file ->
      let st = Unix.lstat file in
      if st.st_kind = S_REG then
        try
          let time = Var.get_local buf buf_mtime in
          Var.set_local buf buf_mtime st.st_mtime;
          if time <> st.st_mtime then
            (Select.select_yes_or_no frame 
               (Printf.sprintf "%s changed on disk; reload (y/n) ?" 
                    buf.buf_name) 
                (fun bool ->
                   if bool 
                   then reload frame 
                   else Frame.status_modified frame true
                 )) |> ignore
       with _ -> Var.set_local buf buf_mtime st.st_mtime
    )
  with _ -> ()
(*e: function [[Complex.check_file]] *)
    
(*s: function [[Complex.exit_efuns]] *)
let exit_efuns frame =
  let buffers = Utils.list_of_hash (Globals.editor()).edt_buffers in
  save_buffers_and_action frame buffers (fun _ -> 
    (* todo: have some exit hooks? *)
    raise (Common.UnixExit 0)
  )
(*e: function [[Complex.exit_efuns]] *)

(*s: function [[Complex.save_some_buffers]] *)
let save_some_buffers frame =
  let buffers = Utils.list_of_hash (Globals.editor()).edt_buffers in
  save_buffers_and_action frame buffers (fun _ -> ())
(*e: function [[Complex.save_some_buffers]] *)

(*s: function [[Complex.load_buffer]] *)
let load_buffer frame = 
  Multi_buffers.set_previous_frame frame;
  Select.select_filename frame "Find file: " (fun str -> 
    Frame.load_file frame.frm_window str |> ignore
  )
(*e: function [[Complex.load_buffer]] *)

(*s: function [[Complex.insert_file]] *)
let insert_file frame =
  Select.select_filename frame "Insert file: " (fun str ->
    let inc = open_in str in
    Simple.insert_string frame (Utils.read_string inc);
    close_in inc
  )
(*e: function [[Complex.insert_file]] *)

(*s: function [[Complex.write_buffer]] *)
let write_buffer frame = 
  let buf = frame.frm_buffer in
  Select.select_filename frame "Save file as: " (fun str -> 
    Ebuffer.change_name buf str;
    Ebuffer.save buf
  )
(*e: function [[Complex.write_buffer]] *)

(*s: function [[Complex.save_buffer]] *)
let save_buffer frame =
  let buf = frame.frm_buffer in
  match buf.buf_filename with
    Some _ -> Ebuffer.save buf
  | None -> write_buffer frame
(*e: function [[Complex.save_buffer]] *)

(*s: function [[Complex.window_load_buffer]] *)
let window_load_buffer frame = 
  Select.select_filename frame "Find file: " 
    (fun str -> 
      let top_window = Top_window.create ()
          (*(Window.display top_window)*)
      in
      Frame.load_file top_window.window str |> ignore
    )
(*e: function [[Complex.window_load_buffer]] *)

(*s: function [[Complex.window_change_buffer]] *)
let window_change_buffer frame =
  Multi_buffers.select_buffer frame "Switch to buffer in new frame: " 
    (Multi_buffers.get_previous_frame ())
    (fun name ->
      let top_window = Top_window.create ()
           (*"TODO_Display"*) 
      in
      Frame.change_buffer top_window.window name
  )
(*e: function [[Complex.window_change_buffer]] *)

(*s: function [[Complex.change_font]] *)
let change_font frame =
  Minibuffer.create_return frame (Keymap.create ()) "Find font: " "fixed"
    (fun old_frame name ->
      let window = frame.frm_window in
      let _top_window = Window.top window in
      (*WX_xterm.change_font xterm name*)
      failwith "Complex.change_font: TODO"
  ) |> ignore
(*e: function [[Complex.change_font]] *)

(*s: constant [[Complex.display_hist]] *)
(*let display_hist = ref []*)
(*e: constant [[Complex.display_hist]] *)
(*s: function [[Complex.open_display]] *)
let open_display frame =
  failwith "Complex.open_display: TODO"
(*
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
      Top_window.create location display |> ignore
   )
*)
(*e: function [[Complex.open_display]] *)

(*s: function [[Complex.goto_line]] *)
let goto_line frame =
  Select.simple_select frame "goto-line:" (fun name ->
    let line = int_of_string name in
    (*s: save current pos from frame for position history navigation *)
    Simple.save_current_pos frame;
    (*e: save current pos from frame for position history navigation *)
    Text.goto_line frame.frm_buffer.buf_text frame.frm_point (line - 1)
  )
(*e: function [[Complex.goto_line]] *)

(*s: function [[Complex.goto_char]] *)
let goto_char frame =
  Select.simple_select frame "goto-char:" (fun name ->
    let char = int_of_string name in
    Text.set_position frame.frm_buffer.buf_text frame.frm_point char
  )
(*e: function [[Complex.goto_char]] *)


(*s: function [[Complex.get_pos]] *)
let describe_position frame =
  Top_window.message 
    (Window.top frame.frm_window)
    (Printf.sprintf "Char position %d" 
       (Text.get_position frame.frm_buffer.buf_text frame.frm_point))
(*e: function [[Complex.get_pos]] *)

(*s: function [[Complex.mark_at_point]] *)
let mark_at_point frame =
  Ebuffer.set_mark frame.frm_buffer frame.frm_point;
  (*s: save current pos from frame for position history navigation *)
  Simple.save_current_pos frame;
  (*e: save current pos from frame for position history navigation *)
  let top_window = Window.top frame.frm_window in
  Top_window.message top_window "Mark set";
  ()
(*e: function [[Complex.mark_at_point]] *)

(*s: constant [[Complex.umask]] *)
let umask = 
  let old = Unix.umask 0 in 
  Unix.umask old |> ignore;
  old
(*e: constant [[Complex.umask]] *)
  
(*s: constant [[Complex.file_perm]] *)
let file_perm = Store.create "file_perm" string_of_int int_of_string
(*e: constant [[Complex.file_perm]] *)
(*s: function [[Complex.mkdir]] *)
let mkdir frame =
  Select.select_filename frame "Make directory: "
    (fun str -> 
      let file_perm = try Var.get_var frame.frm_buffer file_perm with _ -> 
            0x1ff land (lnot umask) in
      Unix.mkdir str file_perm)
(*e: function [[Complex.mkdir]] *)

(*s: constant [[Complex.eval_history]] *)
let eval_history = ref []
(*e: constant [[Complex.eval_history]] *)
(*s: function [[Complex.eval]] *)
let eval frame =
  Select.select_string frame "Eval:" eval_history "" (fun str ->
    let top_window = Window.top frame.frm_window in
    (* This is not enough: the paths also may have changed. *)
    Top_window.message top_window 
      (*(Dyneval.eval 
        (let len = String.length str in
        if str.[len - 1] = ';' && str.[len -2 ] = ';' then str else
        str ^ " ;;"))
      *)(failwith "Complex.eval: TODO")
    )
(*e: function [[Complex.eval]] *)

(*s: constant [[Complex.variable_hist]] *)
let variable_hist = ref []
(*e: constant [[Complex.variable_hist]] *)
(*s: constant [[Complex.value_hist]] *)
let value_hist = ref []
(*e: constant [[Complex.value_hist]] *)
  
(*s: constant [[Complex.all_vars]] *)
let all_vars = ref None
(*e: constant [[Complex.all_vars]] *)
(*s: function [[Complex.all_variables]] *)
let all_variables frame _ =
  let buf = frame.frm_buffer in
  match !all_vars with
    Some (f,l) when f == frame -> l
  | _ ->
      let list = 
        (Store.list buf.buf_vars) @ 
        (Store.list (Globals.editor()).edt_vars) 
      in
      all_vars := Some (frame, list);
      list
(*e: function [[Complex.all_variables]] *)
  
(*s: function [[Complex.set_local_variable]] *)
let set_local_variable frame = 
  Select.select frame "set_local_variable : " variable_hist
    "" (all_variables frame) (fun s -> s) (fun variable ->
      Select.select_string frame (Printf.sprintf "%s : " variable)
      value_hist "" (fun value ->
          Store.input frame.frm_buffer.buf_vars variable value))
(*e: function [[Complex.set_local_variable]] *)
  
(*s: function [[Complex.set_global_variable]] *)
let set_global_variable frame =
  Select.select frame "set_global_variable : " variable_hist
    "" (all_variables frame) (fun s -> s) (fun variable ->
      Select.select_string frame (Printf.sprintf "%s : " variable)
      value_hist "" (fun value ->
          Store.input (Globals.editor()).edt_vars variable value))
(*e: function [[Complex.set_global_variable]] *)
  
(*s: function [[Complex.get_variable]] *)
let describe_variable frame = 
  Select.select frame "get_variable : " variable_hist "" 
    (all_variables frame)
    (fun s -> s) 
    (fun variable ->
      Top_window.mini_message frame 
        (Printf.sprintf "%s : %s" variable (
          let buf = frame.frm_buffer in
          try
            Store.print buf.buf_vars variable
          with _ ->
            Store.print (Globals.editor()).edt_vars variable)))
(*e: function [[Complex.get_variable]] *)

open Options
  
(*s: constant [[Complex.parameters_hist]] *)
let parameters_hist = ref []
(*e: constant [[Complex.parameters_hist]] *)
  
(*s: function [[Complex.set_parameter]] *)
let set_parameter frame = 
  let parameters = Var.get_global Parameter.parameters_var in
  Select.select frame "set-parameter : " parameters_hist
    "" (Parameter.all_parameters frame) (fun s -> s) (fun variable ->
      Select.select_string frame (Printf.sprintf "%s : " variable)
      value_hist "" (fun value ->
          let (input,print,param) = List.assoc variable parameters
          in
          param =:= input value))
(*e: function [[Complex.set_parameter]] *)
  
(*s: function [[Complex.get_parameter]] *)
let get_parameter frame =
  let parameters = Var.get_global Parameter.parameters_var in  
  Select.select frame "get-parameter : " parameters_hist
    "" (Parameter.all_parameters frame) (fun s -> s) (fun variable ->
      Top_window.mini_message frame 
        (Printf.sprintf "%s : %s" variable (
          let (input,print,param) = List.assoc variable parameters
          in
          print !!param)))
(*e: function [[Complex.get_parameter]] *)

 
(*s: toplevel [[Complex._1]] *)
let _ =
  Hook.add_start_hook (fun () ->
    let edt = Globals.editor() in
      Keymap.add_interactive edt.edt_map "make_directory" mkdir;
      Keymap.add_interactive edt.edt_map "set_local_variable" 
        set_local_variable;
      Keymap.add_interactive edt.edt_map "set_global_variable" 
        set_global_variable;
      Keymap.add_interactive edt.edt_map "set_parameter" set_parameter;
      Keymap.add_interactive edt.edt_map "get_parameter" get_parameter;
      
  )
(*e: toplevel [[Complex._1]] *)
  
(*e: features/complexe.ml *)
