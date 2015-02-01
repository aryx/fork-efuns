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

open Options
open Efuns
open Text
open Frame
open Simple
open Top_window
open Multi_frames
open Utils

let select_yes_or_no frame request action =
  let top_window = Window.top frame.frm_window in
  let map = Keymap.create () in
  let yes_reply mini_frame = 
    Minibuffer.kill mini_frame frame;
    top_window.top_second_cursor <- None;
    action true
  in
  let no_reply  mini_frame = 
    Minibuffer.kill mini_frame frame;
    top_window.top_second_cursor <- None;
    action false
  in
  Keymap.add_binding map [NormalMap, Char.code 'y'] yes_reply;
  Keymap.add_binding map [NormalMap, Char.code 'Y'] yes_reply;
  Keymap.add_binding map [NormalMap, Char.code 'n'] no_reply;
  Keymap.add_binding map [NormalMap, Char.code 'N'] no_reply;
  let mini = Minibuffer.create frame map request in
  mini.frm_buffer.buf_map_partial <- false;
  mini

let find_completion_frame frame =
  let location = frame.frm_buffer.buf_location in
  let buf = Ebuffer.default location "*Completions*" in
  Frame.find_buffer_frame location buf

let display_completions frame list =
  let top_window = Window.top frame.frm_window in
  if list = [] then
    Top_window.message top_window "No Completions"
  else
  let location = top_window.top_location in
  let rec iter list s =
    match list with
      [] -> s
    | [f] -> Printf.sprintf "%s\n%s" s f
    | f1::f2::tail  ->
        iter tail (Printf.sprintf "%s\n%-40s%s" s f1 f2)
  in
  let buf = Ebuffer.default location "*Completions*" in
  let text = buf.buf_text in
  Text.update text (iter list "Completions :");
  let frame =
    try
      Frame.find_buffer_frame location buf
    with
      Not_found ->
        Frame.create_inactive (cut_frame frame) buf
  in
  ()

let remove_completions frame =
  try
    let frame = find_completion_frame frame in
    remove_frame frame
  with
    Not_found -> ()

let set_history map string history =
  let current = ref 0 in
  Keymap.add_binding map [NormalMap, XK.xk_Up]
    (fun mini_frame ->
      if !current = List.length !history then
        mini_message mini_frame "No previous line in history"
      else
      let ele = list_nth !current !history in
      incr current;
      let buf = mini_frame.frm_buffer in
      let text = buf.buf_text in
      Text.clear text;
      string := ele;
      insert_string mini_frame ele; ()
  );      
  Keymap.add_binding map [NormalMap, XK.xk_Down]
    (fun mini_frame ->
      if !current < 1 then
        mini_message mini_frame "No other line in history"
      else
        begin
          decr current;
          let ele = list_nth !current !history in
          let buf = mini_frame.frm_buffer in
          let text = buf.buf_text in
          Text.clear text;
          string := ele;
          insert_string mini_frame ele; ()
        end
  )

let incremental_mini_buffer frame ismap request default
    incremental_action action =
  let top_window = Window.top frame.frm_window in
  let incremental mini_frame =
    incremental_action frame (Text.to_string mini_frame.frm_buffer.buf_text)
  in
  let incremental_insert mini_frame =
    self_insert_command mini_frame;
    incremental mini_frame
  in
  for key = 32 to 127 do
    Keymap.add_binding ismap [NormalMap, key] incremental_insert
  done;
  Keymap.add_binding ismap [NormalMap, XK.xk_BackSpace] 
    (fun mini_frame -> 
      delete_backspace_char mini_frame;
      incremental mini_frame);
  top_window.top_second_cursor <- Some frame;
  Minibuffer.create_return frame ismap request default
    (fun frame str -> 
      top_window.top_second_cursor <- None;
      action frame str
  )

let select frame request history start completion_fun prefix_fun action =
  let map = Keymap.create () in
  let string = ref "" in
  Keymap.add_binding map [ControlMap, Char.code 'g']
    (fun mini_frame -> 
      remove_completions mini_frame;
      Minibuffer.kill mini_frame frame);
  let completion = ref "_*NoCompletion*_" in
  let completions = ref [] in
  Keymap.add_binding map [NormalMap, XK.xk_Tab]
    (fun mini_frame ->
      if not(!completion == !string) then
        let text = mini_frame.frm_buffer.buf_text in
        completions := completion_fun !string;
        let suffix, n  = common_suffix
            !completions
            (prefix_fun !string) 
        in
        completion := !string;
        Text.insert text mini_frame.frm_point suffix;
        Text.fmove text mini_frame.frm_point (String.length suffix);
        (if n>1 then
            display_completions frame
              (Utils.completion !completions (prefix_fun !string));
        )
      else
(* can be improved, by displaying another part of the completion buffer *)
        display_completions frame
          (Utils.completion !completions (prefix_fun !string))
  );
  set_history map string history;
  let _ = 
    incremental_mini_buffer frame map request start
    (fun frame str -> 
        string := str;
    )
    (fun old_frame str -> 
        (match !history with
            hd :: _ when hd = str -> ()
          | _ ->
              if str <> "" then
                history := str :: !history);
        remove_completions frame;
        action str
    ) in
  ()
  
  (****************************************************
  
               Filename selection
  
  ****************************************************)

let file_hist = ref []

let dont_complete = define_option ["avoid_filenames"] ""
    (list_option string_option) 
  [ ".*\.o"; ".*\.cm.";".*\.cmxa";".*~";".*\.a";"core";"\..*"]
  
let dont_complete_regexps = ref ([],Str.regexp "")
let dont_complete_regexp () =
  let (old,reg) = !dont_complete_regexps in
  if old == !!dont_complete then
    reg
  else
  let reg = Str2.regexp_from_list !!dont_complete in
  dont_complete_regexps := (!!dont_complete, reg);
  reg

let avoid_completion s =
  let bad_regexp = dont_complete_regexp () in
  not (Str.string_match bad_regexp s 0)

let is_userdir string =
  let n = String.length string in
  (n > 1) && (string.[0] = '~') &&
  (try
      let _ = String.rindex string '/' in
      false
    with
      Not_found -> true)

let complete_filename frame good_file filename =
  if is_userdir filename then Parse_file.users
  else
  let filename = Parse_file.string_to_filename filename in
  let dirname = dirname frame filename in
  let file_list = Utils.file_list dirname in
  match file_list with
    a::b::_ -> 
      begin
        match 
          List.fold_left (fun list ele ->
              if good_file ele then ele :: list else list
          ) [] file_list
        with
          [] -> file_list 
            (* if no completion is OK, return files that should have 
          been avoided *)
        | list -> list
      end
  | _ -> file_list

let select_file frame request history start action =
  let map = Keymap.create () in
  let string = ref "" in
  Keymap.add_binding map [ControlMap, Char.code 'g']
    (fun mini_frame -> 
      remove_completions mini_frame;
      Minibuffer.kill mini_frame frame);
  let completion = ref "_*NoCompletion*_" in
  let completions = ref [] in
  Keymap.add_binding map [NormalMap, XK.xk_Tab]
    (fun mini_frame ->
      if not(!completion = !string) then
        let text = mini_frame.frm_buffer.buf_text in
        completions := complete_filename frame avoid_completion !string;
        let suffix, n  = Utils.common_suffix
            !completions
            (Filename.basename !string) 
        in
        let suffix = 
          let s = !string ^ suffix in
          let len = String.length s in
          if len>0 && s.[len - 1] <> '/' then
            try
              let filename = Parse_file.string_to_filename s in
              let dirname = dirname frame filename in
              let basename = Filename.basename filename in
              let stat = Unix.stat (Filename.concat dirname basename) in
              match stat.Unix.st_kind with
                Unix.S_DIR -> suffix ^ "/"
              | _ -> suffix
            with
              _ -> suffix
          else
            suffix
        in
        completion := !string;
        Text.insert text mini_frame.frm_point suffix;
        Text.fmove text mini_frame.frm_point (String.length suffix);
        string := Text.to_string text;
        (if n>1 then
            display_completions frame
              (Utils.completion !completions (Filename.basename !string)))
      else
(* can be improved, by displaying another part of the completion buffer *)
        display_completions frame
          (Utils.completion !completions (Filename.basename !string))
  );
  set_history map string history;
  let _ = incremental_mini_buffer frame map request start
    (fun frame str -> 
      string := str;
  )
  (fun old_frame str -> 
      history := str :: !history;
      remove_completions frame;
      let str = Parse_file.string_to_filename str in
      action str
  ) in
  Keymap.add_binding map [NormalMap, XK.xk_Prior]
    (fun frame ->
      let frame = find_completion_frame frame in
      backward_screen frame);
  Keymap.add_binding map [NormalMap, XK.xk_Next]
    (fun frame ->
      let frame = find_completion_frame frame in
      forward_screen frame);
  Keymap.add_binding map [NormalMap, Char.code '~']
    (fun frame ->
      let buf = frame.frm_buffer in
      let text = buf.buf_text in
      let point = frame.frm_point in
      kill_bol buf point;
      self_insert_command frame;      
      string := to_string text
  );
  Keymap.add_binding map [NormalMap, Char.code '/']
    (fun frame ->
      let buf = frame.frm_buffer in
      let text = buf.buf_text in
      let point = frame.frm_point in
      if bmove_res text point 1 = 1 then
        ( let c = get_char text point in
          fmove text point 1;
          if c = '/' then
            kill_bol buf point);
      self_insert_command frame;
      string := to_string text            
  );
  ()

let select_filename frame request action =
  let top_window = Window.top frame.frm_window in
  let location = top_window.top_location in
  let curdir = Frame.current_dir frame in
  select_file frame request file_hist (Utils.filename_to_string curdir) action


  (****************************************************
  
               Other selection
  
  ****************************************************)

let prev_buffers = ref []
let next_default = ref ""
let set_previous_frame frame = 
  let name = frame.frm_buffer.buf_name in
  next_default := name;
  prev_buffers := name :: (Utils.list_removeq !prev_buffers name)
let get_previous_frame () = !next_default

let buf_hist = ref []
let select_buffer frame request default action =
  select frame (request^"(default :"^ default ^ ") ") buf_hist ""
    (fun _ -> buffer_list frame) (fun s ->s) 
  (fun str ->
      let str = 
        if str = "" then default else str in
      action str)



let select_string frame request history default action =
  let map = Keymap.create () in
  let string = ref "" in
  set_history map string history;
  let _ = Minibuffer.create_return frame map request default
    (fun _ str -> 
      (match !history with
          hd :: _ when hd = str -> ()
        | _ -> if str <> "" then
              history := str :: !history);
      action str) in
  ()


let simple_select frame request action =
  let map = Keymap.create () in
  let _ = Minibuffer.create_return frame map request ""
    (fun _ str -> action str) in
  ()
  
  
