(*s: features/search.ml *)
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
    
(*s: toplevel Search._1 *)
(*let _ = Time.init ()*)
(*e: toplevel Search._1 *)

(*s: constant Search.case_fold *)
(* todo: this should be a buffer variable, not a global *)
let case_fold = ref false
(*e: constant Search.case_fold *)
(*s: function Search.to_regexp *)
let to_regexp flag str =
  match flag with
  | Regexp ->
      (if !case_fold 
       then Str.regexp_case_fold 
       else Str.regexp
      ) str
  | RegexpString ->
      (if !case_fold 
       then Str.regexp_string_case_fold
       else Str.regexp_string
      ) str
(*e: function Search.to_regexp *)

(*s: type Search.query *)
type query = NoQuery | Query of frame * string
(*e: type Search.query *)

(*s: function Search.replace *)
let replace flag frame query str repl =
  let top_window = Window.top frame.frm_window in
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let regexp = to_regexp flag str in
  let session = Text.start_session text in
  let n = ref 0 in
  try
    let last_pos = ref (Text.get_position text point) in
    match flag,query with
      Regexp, NoQuery ->
        while not (Top_window.check_abort frame) do
          let len = Text.search_forward text regexp point in
          let pos = Text.get_position text point in
          if pos = !last_pos then 
            (if Text.fmove_res text point 1 = 0 then raise Exit)
          else begin
              last_pos := pos;
              let result = Text.replace_matched text repl in
              let (_,_matched) = Text.delete_res text point len in
              Text.insert text point result;
              Text.fmove text point (String.length result);
              n := !n + 1
            end
        done
    | RegexpString, NoQuery ->
        let delta = String.length repl in
        while not (Top_window.check_abort frame) do
          let len = Text.search_forward text regexp point in
          let pos = Text.get_position text point in
          if pos = !last_pos then 
            (if Text.fmove_res text point 1 = 0 then raise Exit)
          else begin
              last_pos := pos;
              Text.delete text point len;
              Text.insert text point repl;
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
              let _ = Select.select_yes_or_no frame request 
                  (fun yes ->
                    if yes then
                      begin
                        let result = Text.replace_matched text repl in
                        let (_,_matched) = Text.delete_res text point len in
                        Text.insert text point result;
                        Text.fmove text point (String.length result);
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
              let _ = Select.select_yes_or_no frame request 
                  (fun yes ->
                    if yes then
                      begin
                        Text.delete text point len;
                        Text.insert text point repl;
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
  | Not_found ->
      Text.commit_session text session;
      Top_window.message top_window
        ("Replace "^(string_of_int !n)^" occurences")
  | _ ->
      Text.commit_session text session
(*e: function Search.replace *)


(*s: function Search.no_query *)
let no_query f = f true
(*e: function Search.no_query *)
(*s: function Search.query *)
let query frame request f =
  let top_window = Window.top frame.frm_window in
  top_window.top_second_cursor <- Some frame;
  Select.select_yes_or_no frame request f |> ignore
(*e: function Search.query *)

(*s: constant Search.string_history *)
let string_history = ref []
(*e: constant Search.string_history *)
(*s: function Search.select_replace *)
let select_replace frame request action =
  Select.select_string frame request string_history "" action 
(*e: function Search.select_replace *)

(*s: function Search.replace_string *)
let replace_string frame =
  select_replace frame "Replace string: " (fun str ->
  select_replace frame "with string: "    (fun str2 ->
    replace RegexpString frame NoQuery str str2
  ))
(*e: function Search.replace_string *)

(*s: function Search.query_replace_string *)
let query_replace_string frame =
  select_replace frame "Replace string: " (fun str ->
  select_replace frame "with string: "    (fun str2 ->
    replace RegexpString frame (Query ( frame, "Replace string ? (y/n)"))
        str str2
  ))
(*e: function Search.query_replace_string *)

(*s: function Search.replace_regexp *)
let replace_regexp frame =
  select_replace frame "Replace Regexp: " (fun str ->
  select_replace frame "with string: " (fun str2 ->
    replace Regexp frame NoQuery str str2
  ))
(*e: function Search.replace_regexp *)

(*s: function Search.query_replace_regexp *)
let query_replace_regexp frame =
  select_replace frame "Replace regexp: " (fun str ->
  select_replace frame "with string: "    (fun str2 ->
    replace Regexp frame (Query (frame, "Replace regexp ? (y/n)")) str str2
  ))
(*e: function Search.query_replace_regexp *)

(*s: constant Search.library_regexp *)
let library_regexp = Str.regexp ".*\\.cm[oa]"
(*e: constant Search.library_regexp *)
(*s: function Search.library_file *)
let library_file str =
  Str.string_match library_regexp str 0
(*e: function Search.library_file *)

(*s: function Search.select_lib_filename *)
let select_lib_filename frame request action =
  Select.select frame request Select.file_hist (Frame.current_dir frame)
  (Select.complete_filename frame library_file)
  Filename.basename action
(*e: function Search.select_lib_filename *)

(*s: constant Search.last_search *)
let last_search = ref ""
(*e: constant Search.last_search *)
(*s: function Search.isearch *)
let isearch to_regexp sens frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let ismap = Keymap.create () in

  let point = frame.frm_point in
  let spoint = Text.dup_point text point in
  let orig = Text.get_position text point in

  let sens = ref sens in
  let to_regexp = ref to_regexp in

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
      match !to_regexp, !case_fold with
      | Regexp, true  -> Str.regexp_case_fold !string, 0
      | Regexp, false -> Str.regexp !string, 0
      | RegexpString, true -> Str.regexp_string_case_fold !string, String.length !string
      | RegexpString, false -> Str.regexp_string !string, String.length !string
    in  
    Text.goto_point text point spoint;
    match !sens with
    | Backward -> Text.search_backward text regexp point |> ignore
    | Forward ->  
        let len = Text.search_forward text regexp point in
     (* Printf.printf  "Found at %d len %d" (Text.get_position text point) len;
        print_newline ();*)
        Text.fmove text point len |> ignore
  in
  let set_last mini_frame =
    if !string = "" then begin
      let buf = mini_frame.frm_buffer in
      let text = buf.buf_text in
      let point = mini_frame.frm_point in
      Text.insert text point !last_search;
      Text.fmove text point (String.length !last_search);
      string := !last_search
    end
  in

  (*s: [[Search.isearch()]] key bindings *)
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

  Keymap.add_binding ismap [ControlMap, Char.code 'w']
    (fun mini_frame ->
(*      set_last mini_frame;       *)
      Text.goto_point text spoint point;
      let end_current_word = Simple.end_of_word buf point in
      string := !string ^ end_current_word;
      Simple.insert_string mini_frame end_current_word;
      isearch_s ();
      Minibuffer.update_request mini_frame (request ())
    );

  let _mini_frame =
    Select.incremental_mini_buffer frame ismap (request ()) !string
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
      Simple.move_backward frame 1 |> ignore
     );  
  Keymap.add_binding ismap [NormalMap, XK.xk_Right]
    (fun mini_frame  ->
      Minibuffer.kill mini_frame frame;
      Simple.move_forward frame 1 |> ignore
    );  
  Keymap.add_binding ismap [NormalMap, XK.xk_Down] (kill_and Simple.forward_line);
  Keymap.add_binding ismap [NormalMap, XK.xk_Up] (kill_and Simple.backward_line);
  Keymap.add_binding ismap [ControlMap, Char.code 'a'] 
    (kill_and Simple.beginning_of_line);
  Keymap.add_binding ismap [ControlMap, Char.code 'e'] 
    (kill_and Simple.end_of_line)
  (*e: [[Search.isearch()]] key bindings *)
(*e: function Search.isearch *)




(*s: constant Search.isearch_forward_regexp *)
let isearch_forward_regexp = isearch Regexp Forward
(*e: constant Search.isearch_forward_regexp *)
(*s: constant Search.isearch_forward *)
let isearch_forward = isearch RegexpString Forward
(*e: constant Search.isearch_forward *)
(*s: constant Search.isearch_backward *)
let isearch_backward = isearch RegexpString Backward
(*e: constant Search.isearch_backward *)
(*s: constant Search.isearch_backward_regexp *)
let isearch_backward_regexp = isearch Regexp Backward 
(*e: constant Search.isearch_backward_regexp *)
  
(*e: features/search.ml *)
