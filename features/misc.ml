(*s: features/misc.ml *)
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
open Edit
open Move

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

(*s: function [[Simple.simplify]] *)
let simplify text start point =
  Text.with_dup_point text start (fun start ->
    let rec iter last_c =
      if start < point then
        let c = Text.get_char text start in
        if c = ' ' || c = '\n' || c = '\t' then
          ( Text.delete text start 1;
            iter ' ')
        else
        if last_c = ' ' then
          ( Text.insert text start " ";
            Text.fmove text start 2;
            iter 'a')
        else
          ( Text.fmove text start 1;
            iter 'a')
    in
    iter 'a'
  )
(*e: function [[Simple.simplify]] *)

(*s: constant [[Simple.line_comment]] *)
let line_comment = Store.create_abstr "Fill_mode.line_comment"
(*e: constant [[Simple.line_comment]] *)

(*s: function [[Simple.fill_paragraph]] *)
(* We will have to modify this to handle line_comment soon !! *)
let fill_paragraph frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  text |> Text.with_session (fun () ->
    Text.with_dup_point text point (fun start ->
    backward_paragraph buf start;
    Text.with_dup_point text start (fun fin ->
      forward_paragraph buf fin;

      simplify text start fin;
      Text.insert text start "\n";
      let rec iter count last_space =
        if Text.compare text start fin < 0 then
        if Text.fmove_res text start 1 = 1 then 
          let c = Text.get_char text start in  
            if c = ' ' then (* good, a new space *)
              iter (count+1) 0
          else
          if count > 75 && count <> last_space then 
              begin
              Text.bmove text start (last_space+1);
              Text.delete text start 1;
              Text.insert text start "\n";
              Text.fmove text start 1;
              iter 0 0
              end
            else
              iter (count+1) (last_space+1)
      in
      iter 0 0;  
      Text.insert text fin "\n";
  )))
(*e: function [[Simple.fill_paragraph]] *)
  
(*s: function [[Simple.insert_special_char]] *)
let insert_special_char frame =
  let key = !Top_window.keypressed in
  let char = Char.chr key in
  if char >= 'a' && char <= 'z' 
  then insert_char frame (Char.chr (key - 97))
  else insert_char frame (Char.chr (key - 65))
(*e: function [[Simple.insert_special_char]] *)

(*****************************************************************************)
(* Complexe *)
(*****************************************************************************)

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
    Edit.insert_string frame (Utils.read_string inc);
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

(*s: function [[Complex.goto_line]] *)
let goto_line frame =
  Select.simple_select frame "goto-line:" (fun name ->
    let line = int_of_string name in
    (*s: save current pos from frame for position history navigation *)
    Move.save_current_pos frame;
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

let cursor_position frm =
  let text = frm.frm_buffer.buf_text in
  let point = frm.frm_point in
  let char = Text.get_char text point in
  let coord = Text.point_coord text point in
  Top_window.message 
    (Window.top frm.frm_window)
    (Printf.sprintf "Char: '%c' (%d, #o%o, #x%x) point=%d line=%d column=%d" 
       char 
       (Char.code char) (Char.code char) (Char.code char)
       (Text.get_position text point)
       (coord.Text.c_line + 1) 
       coord.Text.c_col
    )
  

 
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
      Interactive.value_hist "" (fun value ->
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
      Keymap.add_interactive edt.edt_map "set_local_variable" 
        Interactive.set_local_variable;
      Keymap.add_interactive edt.edt_map "set_global_variable" 
        Interactive.set_global_variable;
      Keymap.add_interactive edt.edt_map "set_parameter" set_parameter;
      Keymap.add_interactive edt.edt_map "get_parameter" get_parameter;
    Var.set_global Ebuffer.saved_buffer_hooks [update_time];
  )
(*e: toplevel [[Complex._1]] *)

(*****************************************************************************)
(* Toplevel *)
(*****************************************************************************)

(*s: toplevel [[Simple._1]] *)
let _ =
  (*s: Simple toplevel setup *)
  Action.define_buffer_action "overwrite_mode" (fun buf -> 
      let mode = overwrite_mode in
      if Ebuffer.has_minor_mode buf mode 
      then Ebuffer.del_minor_mode buf mode
      else Ebuffer.set_minor_mode buf mode
  );
  (*e: Simple toplevel setup *)
  Hook.add_start_hook (fun () ->
    let edt = Globals.editor () in
    let gmap = edt.edt_map in

    (*s: [[Simple._]] start hook *)
    (* standard chars *)
    for key = 32 to 127 do
      Keymap.add_binding gmap [NormalMap, key] self_insert_command
    done;
    (*x: [[Simple._]] start hook *)
    let c_q = (ControlMap, Char.code 'q') in
    (* Keymap.add_prefix gmap [c_q]; *)
    for key = 65 to 65+25 do
      Keymap.add_binding gmap [c_q;ControlMap, key] insert_special_char;
    done;
    for key = 97 to 97+25 do
      Keymap.add_binding gmap [c_q;ControlMap, key] insert_special_char;
    done;
    (*x: [[Simple._]] start hook *)
        (* special for AZERTY keyboards *)
        Array.iter (fun (key, char) ->
            Keymap.add_binding gmap [NormalMap, key] (char_insert_command char)
        ) [| 
    (*
            (XK.xk_eacute, 'é');
            (XK.xk_egrave, 'è');
            (XK.xk_ccedilla, 'ç');
            (XK.xk_agrave, 'à');
            (XK.xk_ugrave, 'ù');
            (XK.xk_mu, 'µ'); 
            (XK.xk_sterling, '£');
            (XK.xk_section, '§');
            (XK.xk_degree,  '°');
    *)
            |];
    (*e: [[Simple._]] start hook *)

    Keymap.add_interactive (edt.edt_map) "fondamental_mode" 
      (fun frame -> Ebuffer.set_major_mode frame.frm_buffer 
          Ebuffer.fondamental_mode);

    Var.set_global line_comment ""
  )
(*e: toplevel [[Simple._1]] *)
  
(*e: features/misc.ml *)
