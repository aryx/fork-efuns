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
open Xtypes
open Edit
open Move

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

(*s: function [[Simple.unset_attr]] *)
let unset_attr frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  Text.unset_attrs text
(*e: function [[Simple.unset_attr]] *)

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
(* Keys *)
(*****************************************************************************)
open Options

(*s: function [[Simple.string_to_modifier]] *)
let string_to_modifier s =  
  let mask = ref 0 in
  for i = 0 to String.length s - 1 do
    mask := !mask lor (match s.[i] with
      | 'C' -> controlMask
      | 'A' -> mod1Mask
      | 'M' -> mod1Mask
      | '1' -> mod1Mask
      | _ -> 0
    )
  done;
  !mask
(*e: function [[Simple.string_to_modifier]] *)
  
(*s: constant [[Simple.name_to_keysym]] *)
let name_to_keysym = 
  ("Button1", XK.xk_Pointer_Button1) ::
  ("Button2", XK.xk_Pointer_Button2) ::
  ("Button3", XK.xk_Pointer_Button3) ::
  ("Button4", XK.xk_Pointer_Button4) ::
  ("Button5", XK.xk_Pointer_Button5) ::
  XK.name_to_keysym
(*e: constant [[Simple.name_to_keysym]] *)
  
(*s: function [[Simple.value_to_key]] *)
(* form: SC-Button1 *)
let value_to_key v =
  match v with 
    Value s -> 
      let key, mods = 
        try
          let index = String.index s '-' in
          let mods = String.sub s 0 index in
          let key = String.sub s (index+1) (String.length s - index - 1) in
          key, mods
        with _ -> s, ""
      in
      let key = List.assoc key name_to_keysym in
      let mods = string_to_modifier mods in
      let map = 
        if mods land (controlMask lor mod1Mask) = (controlMask lor mod1Mask)
        then ControlMetaMap else
        if mods land controlMask <> 0 then ControlMap else
        if mods land mod1Mask <> 0 then MetaMap else NormalMap
      in
      map, key
      
  | _ -> raise Not_found
(*e: function [[Simple.value_to_key]] *)
  
(*s: function [[Simple.key_to_value]] *)
let key_to_value k = Value (Keymap.print_key k)
(*e: function [[Simple.key_to_value]] *)
      
(*s: constant [[Simple.key_option]] *)
let key_option = define_option_class "Key" value_to_key key_to_value
(*e: constant [[Simple.key_option]] *)

(*s: constant [[Simple.binding_option]] *)
let binding_option = tuple2_option (smalllist_option key_option, string_option)
(*e: constant [[Simple.binding_option]] *)

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
