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

open Top_window
open Efuns
open Text
open Frame
open Simple
open Select
    
let _ = Time.init ()

let case_fold = ref false
let to_regexp flag str =
  match flag with
    Regexp ->
      (if !case_fold then
          Str.regexp_case_fold 
        else
          Str.regexp) str
  | RegexpString ->
      (if !case_fold then
          Str.regexp_string_case_fold
        else
          Str.regexp_string) str

type query = NoQuery | Query of frame * string

let replace flag frame query str repl =
  let top_window = Window.top frame.frm_window in
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let regexp = to_regexp flag str in
  let session = start_session text in
  let n = ref 0 in
  try
    let last_pos = ref (Text.get_position text point) in
    match flag,query with
      Regexp, NoQuery ->
        while not (check_abort frame) do
          let len = Text.search_forward text regexp point in
          let pos = Text.get_position text point in
          if pos = !last_pos then 
            (if Text.fmove_res text point 1 = 0 then raise Exit)
          else begin
              last_pos := pos;
              let result = Text.replace_matched text repl in
              let (_,matched) = delete_res text point len in
              insert text point result;
              fmove text point (String.length result);
              n := !n + 1
            end
        done
    | RegexpString, NoQuery ->
        let delta = String.length repl in
        while not (check_abort frame) do
          let len = Text.search_forward text regexp point in
          let pos = Text.get_position text point in
          if pos = !last_pos then 
            (if Text.fmove_res text point 1 = 0 then raise Exit)
          else begin
              last_pos := pos;
              delete text point len;
              insert text point repl;
              Text.fmove text point delta;
              n := !n + 1
            end
        done
    | Regexp, Query (frame,request) ->
        let top_window = Window.top frame.frm_window in
        let rec iter () =
          let len = Text.search_forward text regexp point in
          let pos = Text.get_position text point in
          if pos = !last_pos then begin
              (if Text.fmove_res text point 1 = 0 then raise Exit);
              iter ();
            end
          else begin
              last_pos := pos;
              top_window.top_second_cursor <- Some frame;
              let _ = select_yes_or_no frame request 
                  (fun yes ->
                    if yes then
                      begin
                        let result = Text.replace_matched text repl in
                        let (_,matched) = delete_res text point len in
                        insert text point result;
                        fmove text point (String.length result);
                        n := !n + 1
                      end;
                    iter ()) in
              ()
            end
        in
        iter ()
    | RegexpString, Query (frame,request) ->
        let delta = String.length repl in
        let top_window = Window.top frame.frm_window in
        let rec iter () =
          let len = Text.search_forward text regexp point in
          let pos = Text.get_position text point in
          if pos = !last_pos then begin
              (if Text.fmove_res text point 1 = 0 then raise Exit);
              iter ();
            end
          else begin
              last_pos := pos;
              top_window.top_second_cursor <- Some frame;
              let _ = select_yes_or_no frame request 
                  (fun yes ->
                    if yes then
                      begin
                        delete text point len;
                        insert text point repl;
                        Text.fmove text point delta;
                        n := !n + 1
                      end;
                    iter ()) 
              in
              ()
            end
        in
        iter ()
  with
    Not_found ->
      commit_session text session;
      Top_window.message top_window
        ("Replace "^(string_of_int !n)^" occurences")
  | _ ->
      commit_session text session


let no_query f = f true
let query frame request f =
  let top_window = Window.top frame.frm_window in
  top_window.top_second_cursor <- Some frame;
  let _ = select_yes_or_no frame request f in ()

let string_history = ref []
let select_replace frame request action =
  select_string frame request string_history "" action 

let replace_string frame =
  select_replace frame "Replace string: " 
    (fun str ->
      select_replace frame "with string: " 
        (replace RegexpString frame NoQuery str))

let query_replace_string frame =
  select_replace frame "Replace string: " 
    (fun str ->
      select_replace frame "with string: " 
        (replace RegexpString frame 
          (Query ( frame, "Replace string ? (y/n)"))
        str)
  )

let replace_regexp frame =
  select_replace frame "Replace Regexp: " 
    (fun str ->
      select_replace frame "with string: " 
        (replace Regexp frame NoQuery str))

let query_replace_regexp frame =
  select_replace frame "Replace regexp: " 
    (fun str ->
      select_replace frame "with string: " 
        (replace Regexp frame 
          (Query (frame, "Replace regexp ? (y/n)"))
        str))

let library_regexp = Str.regexp ".*\.cm[oa]"
let library_file str =
  Str.string_match library_regexp str 0

let select_lib_filename frame request action =
  let top_window = Window.top frame.frm_window in
  let location = top_window.top_location in
  select frame request file_hist (current_dir frame)
  (complete_filename frame library_file)
  Filename.basename action

let last_search = ref ""
let isearch to_regexp sens frame =
  let top_window = Window.top frame.frm_window in
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let spoint = Text.dup_point text point in
  let orig = Text.get_position text point in
  let sens = ref sens in
  let to_regexp = ref to_regexp in
  let ismap = Keymap.create () in
  let request () =
    "isearch-"^
      (match !sens with
        Backward -> "backward"
      | Forward -> "forward"
    )^
      (match !to_regexp with
        Regexp -> "-regexp :"
      | RegexpString -> " :"
    )
  in
  let string = ref "" in
  let isearch_s () =
    last_search := !string;
    let regexp, delta =       
      if !case_fold then
        match !to_regexp with
          Regexp -> Str.regexp_case_fold !string, 0
        | RegexpString -> Str.regexp_string_case_fold
              !string, String.length !string
      else
      match !to_regexp with
        Regexp -> Str.regexp !string, 0
      | RegexpString -> Str.regexp_string !string, String.length !string
    in  
    let point = frame.frm_point in
    let buf = frame.frm_buffer in
    let text = buf.buf_text in
    goto_point text point spoint;
    match !sens with
      Backward -> 
        let _ = Text.search_backward text regexp point in ()
    | Forward ->  
        let len = Text.search_forward text regexp point in
(*        Printf.printf  "Found at %d len %d" (Text.get_position text point) len;
        print_newline ();*)
        fmove text point len; ()
  in
  let set_last mini_frame =
    if !string = "" then
      let buf = mini_frame.frm_buffer in
      let text = buf.buf_text in
      let point = mini_frame.frm_point in
      Text.insert text point !last_search;
      Text.fmove text point (String.length !last_search);
      string := !last_search
  in
  Keymap.add_binding ismap [ControlMap, Char.code 's'] 
    (fun mini_frame ->
      set_last mini_frame;      
      Text.goto_point text spoint point;
      sens := Forward;
      isearch_s ();
      Minibuffer.update_request mini_frame (request ())
  );    
  Keymap.add_binding ismap [ControlMap, Char.code 'r']
    (fun mini_frame ->
      set_last mini_frame;      
      Text.goto_point text spoint point;
      sens := Backward;
      isearch_s ();
      Minibuffer.update_request mini_frame (request ())
  );  
  let mini_frame =
    incremental_mini_buffer frame ismap (request ()) !string
      (fun frame str -> 
        string := str;
        isearch_s ()
    )
    (fun frame str -> 
        last_search := str;
        Text.remove_point text spoint
    )
  in
  let kill_and f mini_frame =
    last_search := Text.to_string mini_frame.frm_buffer.buf_text;
    Minibuffer.kill mini_frame frame;
    f frame
  in
  Keymap.add_binding ismap [ControlMap, Char.code 'g']
    (fun mini_frame  ->
      Minibuffer.kill mini_frame frame;
      Text.set_position frame.frm_buffer.buf_text frame.frm_point orig
  );
  Keymap.add_binding ismap [NormalMap, XK.xk_Left]
    (fun mini_frame  ->
      Minibuffer.kill mini_frame frame;
      let _ = move_backward frame 1 in ());  
  Keymap.add_binding ismap [NormalMap, XK.xk_Right]
    (fun mini_frame  ->
      Minibuffer.kill mini_frame frame;
      let _ = move_forward frame 1 in ());  
  Keymap.add_binding ismap [NormalMap, XK.xk_Down] (kill_and forward_line);
  Keymap.add_binding ismap [NormalMap, XK.xk_Up] (kill_and backward_line);
  Keymap.add_binding ismap [ControlMap, Char.code 'a'] 
    (kill_and beginning_of_line);
  Keymap.add_binding ismap [ControlMap, Char.code 'e'] 
    (kill_and end_of_line)




let isearch_forward_regexp = isearch Regexp Forward
let isearch_forward = isearch RegexpString Forward
let isearch_backward = isearch RegexpString Backward
let isearch_backward_regexp = isearch Regexp Backward 
  