(*s: features/simple.ml *)
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

(*****************************************************************************)
(* Insertion *)
(*****************************************************************************)

(*s: function Simple.insert_string *)
let insert_string frame str =
  let text = frame.frm_buffer.buf_text in
  let point = frame.frm_point in
  Text.insert text point str |> ignore;
  Text.fmove text point (String.length str) |> ignore
(*e: function Simple.insert_string *)
  
(*s: constant Simple.single_char *)
let single_char = String.make 1 ' '
(*e: constant Simple.single_char *)
(*s: function Simple.insert_char *)
(* could factorize and just call insert_string? *)
let insert_char frame char =
  let text = frame.frm_buffer.buf_text in
  let point = frame.frm_point in
  single_char.[0] <- char;
  Text.insert text point single_char |> ignore;
  Text.fmove text point 1 |> ignore
(*e: function Simple.insert_char *)

(*s: function Simple.insert_return *)
let insert_return frame =
  insert_char frame '\n'
(*e: function Simple.insert_return *)

(*s: function Simple.previous_char *)
let previous_char frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  if Text.bmove_res text point 1 = 0 then raise Not_found;
  let c = Text.get_char text point in
  Text.fmove text point 1 |> ignore;
  c
(*e: function Simple.previous_char *)

(*s: function Simple.unset_attr *)
let unset_attr frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  Text.unset_attr text
(*e: function Simple.unset_attr *)
  
(*s: function Simple.insert_at_place *)
let insert_at_place frame char =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let c = Text.get_char text point in
  if c = '\n' then
    insert_char frame char
  else
  Text.delete text point 1 |> ignore;
  single_char.[0] <- char;    
  Text.insert text point single_char |> ignore;
  Text.fmove text point 1 |> ignore
(*e: function Simple.insert_at_place *)


(*s: constant Simple.overwrite_mode *)
let overwrite_mode = Ebuffer.new_minor_mode "Over" []
(*e: constant Simple.overwrite_mode *)
  
  
(*s: function Simple.self_insert_command *)
let self_insert_command frame =
  let char = Char.chr !Top_window.keypressed in
  let buf = frame.frm_buffer in
  if Ebuffer.modep buf overwrite_mode 
  then insert_at_place frame char
  else insert_char frame char
(*e: function Simple.self_insert_command *)
    
(*s: function Simple.char_insert_command *)
let char_insert_command char frame =
  let buf = frame.frm_buffer in
  if Ebuffer.modep buf overwrite_mode 
  then insert_at_place frame char
  else insert_char frame char
(*e: function Simple.char_insert_command *)

(*****************************************************************************)
(* Deletion *)
(*****************************************************************************)

(*s: function Simple.delete_char *)
let delete_char frame =
  let text = frame.frm_buffer.buf_text in
  Text.delete text frame.frm_point 1 |> ignore
(*e: function Simple.delete_char *)

(*s: function Simple.delete_backspace_char *)
let delete_backspace_char frame =
  let text = frame.frm_buffer.buf_text in
  if Text.bmove_res text frame.frm_point 1 <> 0 
  then Text.delete text frame.frm_point 1 |> ignore
(*e: function Simple.delete_backspace_char *)

(*s: function Simple.hungry_char *)
let hungry_char c = 
  c = ' ' || c = '\n' || c = '\t'
(*e: function Simple.hungry_char *)

(*s: function Simple.hungry_electric_delete *)
let hungry_electric_delete frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  text |> Text.with_session (fun () ->
   let c1 = previous_char frame in
   delete_backspace_char frame;
   let c2 = previous_char frame in
   if hungry_char c1 && hungry_char c2 then
     try
       delete_backspace_char frame;
       while 
         let c = previous_char frame in
         hungry_char c        
       do
         delete_backspace_char frame
       done;
       insert_char frame ' '
     with Not_found -> ()
  )
(*e: function Simple.hungry_electric_delete *)

(*****************************************************************************)
(* Navigation *)
(*****************************************************************************)

(*s: function Simple.move_backward *)
let move_backward frame delta =
  Text.bmove frame.frm_buffer.buf_text frame.frm_point delta
(*e: function Simple.move_backward *)

(*s: function Simple.move_forward *)
let move_forward frame delta =
  Text.fmove frame.frm_buffer.buf_text frame.frm_point delta
(*e: function Simple.move_forward *)

(*s: function Simple.begin_to_point *)
let begin_to_point frame =
  Text.point_to_bol frame.frm_buffer.buf_text frame.frm_point
(*e: function Simple.begin_to_point *)

(*s: function Simple.point_to_end *)
let point_to_end frame =
  Text.point_to_eol frame.frm_buffer.buf_text frame.frm_point
(*e: function Simple.point_to_end *)


(*s: function Simple.line_size *)
let line_size frame =
  (point_to_end frame) + (point_to_end frame)
(*e: function Simple.line_size *)

(*s: function Simple.beginning_of_line *)
let beginning_of_line frame =
  move_backward frame (begin_to_point frame) |> ignore
(*e: function Simple.beginning_of_line *)

(*s: function Simple.end_of_line *)
let end_of_line frame =
  move_forward frame (point_to_end frame) |> ignore
(*e: function Simple.end_of_line *)


(*s: constant Simple.temporary_goal_column *)
let temporary_goal_column = 
  Local.create_abstr "Simple.temporary_goal_column"
(*e: constant Simple.temporary_goal_column *)

(*s: function Simple.goal_column *)
let rec goal_column frame =
  let cur_col = begin_to_point frame in
  if frame.frm_last_action == forward_line ||
     frame.frm_last_action == backward_line
  then 
    try Var.get_local frame.frm_buffer temporary_goal_column
    with Not_found -> cur_col
  else cur_col
(*e: function Simple.goal_column *)

(*s: function Simple.move_to_goal_column *)
and move_to_goal_column frame goal_col =
  move_backward frame (begin_to_point frame) |> ignore;
  move_forward frame (min goal_col (point_to_end frame)) |> ignore;
  Var.set_local frame.frm_buffer temporary_goal_column goal_col
(*e: function Simple.move_to_goal_column *)

(*s: function Simple.forward_line *)
and forward_line frame =
  let text = frame.frm_buffer.buf_text in
  let point = frame.frm_point in
  if Text.point_line text point < Text.nbre_lines text then begin
    let goal_col = goal_column frame in
    end_of_line frame;
    move_forward frame 1 |> ignore;
    move_to_goal_column frame goal_col;
  end
(*e: function Simple.forward_line *)

(*s: function Simple.backward_line *)
and backward_line frame =
  let text = frame.frm_buffer.buf_text in
  let point = frame.frm_point in
  if Text.point_line text point > 0 then begin
    let goal_col = goal_column frame in
    beginning_of_line frame;
    move_backward frame 1 |> ignore;
    move_to_goal_column frame goal_col;
  end
(*e: function Simple.backward_line *)


(*****************************************************************************)
(* Kill *)
(*****************************************************************************)

(*s: constant Simple.kill_size *)
let kill_size = ref 0
(*e: constant Simple.kill_size *)
(*s: constant Simple.kill_max *)
let kill_max = 10
(*e: constant Simple.kill_max *)
(*s: constant Simple.kill_ring *)
let kill_ring = Array.create kill_max ""
(*e: constant Simple.kill_ring *)
(*s: constant Simple.last_kill *)
let last_kill = ref None
(*e: constant Simple.last_kill *)
(*s: constant Simple.last_insert *)
let last_insert = ref None
(*e: constant Simple.last_insert *)

(*s: function Simple.kill_string *)
let kill_string str =
  Array.blit kill_ring 0 kill_ring 1 (kill_max - 1);
  incr kill_size;
  kill_ring.(0) <- str
(*e: function Simple.kill_string *)


(*s: function Simple.kill_text *)
let kill_text text point len =
  let point,str = Text.delete_res text point len in
  match !last_kill with
  | Some (oldtext,oldpoint) when
    oldpoint = point && oldtext == text ->
      kill_ring.(0) <- kill_ring.(0)^str
  | _ ->
      last_kill := Some (text,point);
      kill_string str
(*e: function Simple.kill_text *)

(*s: function Simple.kill_end_of_line *)
let kill_end_of_line frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let eol = point_to_end frame in
  let len = if eol = 0 then 1 else eol in
  kill_text text frame.frm_point len
(*e: function Simple.kill_end_of_line *)

(*s: function Simple.kill_eol *)
let kill_eol buf point =
  let text = buf.buf_text in
  let eol = Text.point_to_eol text point in
  let len =
    if eol = 0 then 1 else eol
  in
  kill_text text point len
(*e: function Simple.kill_eol *)

(*s: function Simple.kill_bol *)
let kill_bol buf point =
  let text = buf.buf_text in
  let len = Text.point_to_bol text point in
  if len > 0 then
    ( Text.bmove text point len;
      kill_text text point len)
(*e: function Simple.kill_bol *)

(*s: function Simple.insert_killed *)
let insert_killed frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let pos, len =  Text.insert_res text point kill_ring.(0) in
  Text.fmove text point len; 
  last_insert := Some(frame,pos,0,len)
(*e: function Simple.insert_killed *)

(*s: function Simple.insert_next_killed *)
let insert_next_killed frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  match !last_insert with
  |  Some (oldframe,oldpoint,n,len) when 
        oldframe == frame && oldpoint + len = Text.get_position text point ->
      let n = 
        if n = (min !kill_size kill_max) - 1 
        then 0 
        else n+1 
      in
      Text.bmove text point len;
      Text.delete text point len |> ignore;
      let pos, len =  Text.insert_res text point kill_ring.(n) in
      Text.fmove text point len;
      last_insert := Some(frame,pos,n,len)
  | _ -> ()
(*e: function Simple.insert_next_killed *)


(*s: function Simple.kill_region *)
let kill_region frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let mark =
    match buf.buf_mark with
    | None -> failwith "No mark set"
    | Some mark -> mark
  in
  let (start,term) = 
    if mark > point then (point,mark) else (mark,point)
  in
  let _,region = Text.delete_res text start (Text.distance text start term) in
  kill_string region
(*e: function Simple.kill_region *)

(*s: function Simple.copy_region *)
(* copy-region-as-kill-nomark in emacs *)
let copy_region frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let mark =
    match buf.buf_mark with
      None -> failwith "No mark set"
    | Some mark -> 
        buf.buf_mark <- None;
        mark
  in
  let (start,term) = 
    if mark > point then (point,mark) else (mark,point)
  in
  let region = Text.sub text start (Text.distance text start term) in
  kill_string region;
  let top_window = Window.top frame.frm_window in
  Top_window.message top_window "Region saved"
(*e: function Simple.copy_region *)
  

(*****************************************************************************)
(* Words *)
(*****************************************************************************)

(*s: function Simple.in_next_word *)
let in_next_word text mark syntax =
  while (not syntax.(Char.code (Text.get_char text mark))) &&
        Text.fmove_res text mark 1 = 1 
  do () done
(*e: function Simple.in_next_word *)

(*s: function Simple.in_prev_word *)
let in_prev_word text mark syntax =
  while Text.bmove_res text mark 1 = 1 &&
        (not syntax.(Char.code (Text.get_char text mark)))
  do () done
(*e: function Simple.in_prev_word *)


(*s: function Simple.to_begin_of_word *)
let to_begin_of_word text mark syntax =
  if Text.bmove_res text mark 1 = 1 then
    begin
      while syntax.(Char.code (Text.get_char text mark)) &&
            (Text.bmove_res text mark 1) <> 0 
      do () done;
      if not syntax.(Char.code (Text.get_char text mark)) 
      then (Text.fmove text mark 1)
    end
(*e: function Simple.to_begin_of_word *)

(*s: function Simple.to_end_of_word *)
let to_end_of_word text mark syntax =
  while syntax.(Char.code (Text.get_char text mark)) &&
       (Text.fmove_res text mark 1) <> 0 
  do () done
(*e: function Simple.to_end_of_word *)


(*s: function Simple.backward_word *)
let backward_word buf point =
  let text = buf.buf_text in
  let syntax = buf.buf_syntax_table in
  in_prev_word text point syntax;
  to_begin_of_word text point syntax
(*e: function Simple.backward_word *)

(*s: function Simple.forward_word *)
let forward_word buf point =
  let text = buf.buf_text in
  let syntax = buf.buf_syntax_table in
  in_next_word text point syntax;
  to_end_of_word text point syntax
(*e: function Simple.forward_word *)

(*s: function Simple.beginning_of_word *)
let beginning_of_word buf point =
  let text = buf.buf_text in
  let syntax = buf.buf_syntax_table in
  let mark = Text.dup_point text point in
  to_begin_of_word text mark syntax;
  let s = Text.region text mark point in
  Text.remove_point text mark;
  s
(*e: function Simple.beginning_of_word *)

(*s: function Simple.end_of_word *)
let end_of_word  buf point =
  let text = buf.buf_text in
  let syntax = buf.buf_syntax_table in
  let mark = Text.dup_point text point in
  to_end_of_word text mark syntax;
  let s = Text.region text point mark in
  Text.remove_point text mark;
  s
(*e: function Simple.end_of_word *)

(*s: function Simple.current_word *)
let current_word buf point =
  (beginning_of_word buf point) ^ (end_of_word buf point)
(*e: function Simple.current_word *)
 
(*s: function Simple.delete_backward_word *)
let delete_backward_word buf point =
  let text = buf.buf_text in
  let old_point = Text.dup_point text point in
  backward_word buf point;
  Text.delete text point (Text.distance text point old_point) |> ignore;
  Text.remove_point text old_point
(*e: function Simple.delete_backward_word *)

(*s: function Simple.delete_forward_word *)
let delete_forward_word buf point =
  let text = buf.buf_text in
  let old_point = Text.dup_point text point in
  forward_word buf point;
  let len = Text.distance text old_point point in
  Text.remove_point text old_point;
  Text.bmove text point len;
  Text.delete text point len |> ignore
(*e: function Simple.delete_forward_word *)

(*s: function Simple.on_word *)
let on_word buf point f =
  let text = buf.buf_text in
  text |> Text.with_session (fun () ->
    let syntax = buf.buf_syntax_table in
    to_begin_of_word text point syntax;
    let pos1 = Text.dup_point text point in
    to_end_of_word text point syntax;
    let _,word1 = Text.delete_res text pos1 (Text.distance text pos1 point) in
    let w = f word1 in
    Text.insert text pos1 w |> ignore;
    Text.fmove text point (String.length w);
    Text.remove_point text pos1
  )
(*e: function Simple.on_word *)
  
(*s: function Simple.transpose_words *)
let transpose_words buf point =
  let text = buf.buf_text in
  text |> Text.with_session (fun () ->
    let syntax = buf.buf_syntax_table in
    in_prev_word text point syntax;
    to_begin_of_word text point syntax;
    let pos1 = Text.dup_point text point in
    to_end_of_word text point syntax;
    let _,word1 = Text.delete_res text pos1 (Text.distance text pos1 point) in
    Text.goto_point text point pos1;
    in_next_word text point syntax;
    let pos2 = Text.dup_point text point in
    to_end_of_word text point syntax;
    let _,word2 = Text.delete_res text pos2 (Text.distance text pos2 point) in    
    Text.insert text pos1 word2 |> ignore;
    Text.insert text pos2 word1 |> ignore;
    Text.fmove text point (String.length word1);
    Text.remove_point text pos1;
    Text.remove_point text pos2
  )
(*e: function Simple.transpose_words *)


(*s: function Simple.transpose_chars *)
let transpose_chars buf point =
  let text = buf.buf_text in
  text |> Text.with_session (fun () ->
    Text.bmove text point 1;
    let pos,c1 = Text.delete_res text point 1 in
    Text.fmove text point 1;
    Text.insert text point c1 |> ignore;
  );
  Text.fmove text point 1;
  ()
(*e: function Simple.transpose_chars *)

(*****************************************************************************)
(* Paragraphs *)
(*****************************************************************************)

(*s: function Simple.backward_paragraph *)
let backward_paragraph buf point =
  let text = buf.buf_text in
  while Text.bmove_res text point 1 = 1 && 
        (let c = Text.get_char text point in c = '\n' || c = ' ')
  do () done;
  try
    Text.search_backward text (Str.regexp "\n *\n") point |> ignore;
    Text.fmove text point 1
  with Not_found -> Text.set_position text point 0
(*e: function Simple.backward_paragraph *)

(*s: function Simple.forward_paragraph *)
let forward_paragraph buf point =
  let text = buf.buf_text in
  while (let c = Text.get_char text point in c = '\n' || c = ' ') &&
         Text.fmove_res text point 1 = 1 
  do () done;
  try
    Text.search_forward text (Str.regexp "\n *\n") point |> ignore;
    Text.fmove text point 1
  with Not_found -> Text.set_position text point (Text.size text)
(*e: function Simple.forward_paragraph *)

(*****************************************************************************)
(* Scroll *)
(*****************************************************************************)

(*
let move_backward frame =
   Functions.move_backward frame 1; ()

let move_forward frame = 
  Functions.move_forward frame 1; () 
*) 
  
(*s: function Simple.forward_screen *)
let forward_screen frame =
  frame.frm_force_start <- true;
  frame.frm_redraw <- true;
  frame.frm_y_offset <- frame.frm_y_offset + frame.frm_height - 2
(*e: function Simple.forward_screen *)

(*s: function Simple.backward_screen *)
let backward_screen frame =
  frame.frm_force_start <- true;
  frame.frm_redraw <- true;
  frame.frm_y_offset <- frame.frm_y_offset - frame.frm_height + 2
(*e: function Simple.backward_screen *)

(*s: function Simple.scroll_line *)
let scroll_line frame n =
  frame.frm_force_start <- true;
  frame.frm_redraw <- true;
  frame.frm_y_offset <- frame.frm_y_offset + n
(*e: function Simple.scroll_line *)

(*s: function Simple.scroll_down *)
let scroll_down frame =
  scroll_line frame 1;
  forward_line frame
(*e: function Simple.scroll_down *)

(*s: function Simple.scroll_up *)
let scroll_up frame =
  scroll_line frame (-1);
  backward_line frame
(*e: function Simple.scroll_up *)

(*s: function Simple.scroll_other_up *)
let scroll_other_up frame =
  Window.next scroll_up frame.frm_window
(*e: function Simple.scroll_other_up *)

(*s: function Simple.scroll_other_down *)
let scroll_other_down frame =
  Window.next scroll_down frame.frm_window
(*e: function Simple.scroll_other_down *)

(*s: function Simple.recenter *)
let recenter frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  frame.frm_force_start <- true;
  frame.frm_redraw <- true;
  Text.goto_point text frame.frm_start frame.frm_point;
  frame.frm_y_offset <- - frame.frm_height/2
(*e: function Simple.recenter *)

(*****************************************************************************)
(* Position history *)
(*****************************************************************************)

(*s: constant Simple.history_pos_max *)
let history_pos_max = 10
(*e: constant Simple.history_pos_max *)

(*s: function Simple.save_current_pos *)
let save_current_pos frame =
  let buf = frame.frm_buffer in
  let point = frame.frm_point in
  if Array.length buf.buf_history_pos < history_pos_max
  then buf.buf_history_pos <- Array.create history_pos_max None;
  let arr = buf.buf_history_pos in
  Array.blit arr 0 arr 1 (history_pos_max - 1);
  arr.(0) <- Some (Text.dup_point buf.buf_text point)
(*e: function Simple.save_current_pos *)

(*s: function Simple.goto_last_saved_pos *)
let goto_last_saved_pos frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  if Array.length buf.buf_history_pos < history_pos_max
  then buf.buf_history_pos <- Array.create history_pos_max None;
  let arr = buf.buf_history_pos in
  let head = arr.(0) in
  Array.blit arr 1 arr 0 (history_pos_max - 1);
  match head with
  | Some pt -> Text.goto_point text frame.frm_point pt
  | None -> failwith "No position history"
(*e: function Simple.goto_last_saved_pos *)

(*****************************************************************************)
(* File *)
(*****************************************************************************)

(*s: function Simple.end_of_file *)
let end_of_file frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  (*s: save current pos from frame for position history navigation (in simple.ml) *)
  save_current_pos frame;
  (*e: save current pos from frame for position history navigation (in simple.ml) *)
  Text.set_position text frame.frm_point (Text.size text)
(*e: function Simple.end_of_file *)

(*s: function Simple.begin_of_file *)
let begin_of_file frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  (*s: save current pos from frame for position history navigation (in simple.ml) *)
  save_current_pos frame;
  (*e: save current pos from frame for position history navigation (in simple.ml) *)
  Text.set_position text frame.frm_point 0
(*e: function Simple.begin_of_file *)

(*****************************************************************************)
(* Undo *)
(*****************************************************************************)

(*s: function Simple.undo *)
let undo frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in  
  let point = frame.frm_point in
  let action, at_point, len = Text.undo text in
  frame.frm_last_text_updated <- Text.version text - 1;
  (*s: save current pos from frame for position history navigation (in simple.ml) *)
  save_current_pos frame;
  (*e: save current pos from frame for position history navigation (in simple.ml) *)
  Text.set_position text point at_point;
  Text.fmove text point len
(*e: function Simple.undo *)

(*****************************************************************************)
(* Color helpers *)
(*****************************************************************************)

(*s: function Simple.color *)
let color buf regexp strict attr =
  let text = buf.buf_text in
  let point = Text.new_point text in
  try
    while true do
      let len = Text.search_forward text regexp point in
      let before =
        if Text.bmove_res text point 1 = 1 then begin
          let c = Text.get_char text point in
          Text.fmove text point (len+1);
          c
        end else begin
          let c = Text.get_char text point in
          Text.fmove text point (len+1); 
          c
        end
      in
      let after = Text.get_char text point in
      if not (strict && (buf.buf_syntax_table.(Char.code before) ||
            buf.buf_syntax_table.(Char.code after))) then
        begin
          Text.bmove text point len;
          Text.set_attr text point len attr;
          Text.fmove text point len;
          ()
        end
    done
  (* at some point Text.search_forward will return Not_found *)
  with Not_found -> 
    Text.remove_point text point;
    buf.buf_modified <- buf.buf_modified + 1
(*e: function Simple.color *)

(*****************************************************************************)
(* Points *)
(*****************************************************************************)

(*s: function Simple.point_at_mark *)
let point_at_mark frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in

  let mark = Ebuffer.get_mark buf point in
  let pos = Text.get_position text point in
  Text.goto_point text point mark;
  Text.set_position text mark pos
(*e: function Simple.point_at_mark *)


(*****************************************************************************)
(* Electric *)
(*****************************************************************************)

(*s: function Simple.electric_insert_space *)
let electric_insert_space frame =
  self_insert_command frame;
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let line_len = Text.point_to_bol text point in
  if line_len > 75 then
    let mark = Text.dup_point text point in
    (try
        while (backward_word buf mark;
            Text.point_to_bol text mark > 75) do () done;
        forward_word buf mark; backward_word buf mark;
        Text.insert text mark "\n" |> ignore
    with Not_found -> ()
    );
    Text.remove_point text mark
(*e: function Simple.electric_insert_space *)

(*s: function Simple.simplify *)
let simplify text start point =
  let start = Text.dup_point text start in
  let rec iter last_c =
    if start < point then
      let c = Text.get_char text start in
      if c = ' ' || c = '\n' || c = '\t' then
        ( Text.delete text start 1 |> ignore;
          iter ' ')
      else
      if last_c = ' ' then
        ( Text.insert text start " " |> ignore;
          Text.fmove text start 2;
          iter 'a')
      else
        ( Text.fmove text start 1;
          iter 'a')
  in
  iter 'a';
  Text.remove_point text start
(*e: function Simple.simplify *)

(*s: constant Simple.line_comment *)
let line_comment = Local.create_abstr "Fill_mode.line_comment"
(*e: constant Simple.line_comment *)

(*s: function Simple.fill_paragraph *)
(* We will have to modify this to handle line_comment soon !! *)
let fill_paragraph frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let start = Text.dup_point text point in
  text |> Text.with_session (fun () ->
    backward_paragraph buf start;
    let fin = Text.dup_point text start in
    forward_paragraph buf fin;
    simplify text start fin;
    Text.insert text start "\n" |> ignore;
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
    Text.insert text fin "\n" |> ignore;
    Text.remove_point text start;
    Text.remove_point text fin;
  )
(*e: function Simple.fill_paragraph *)
  
(*s: function Simple.insert_special_char *)
let insert_special_char frame =
  let key = !Top_window.keypressed in
  let char = Char.chr key in
  if char >= 'a' && char <= 'z' then
    insert_char frame (Char.chr (key - 97))
  else
    insert_char frame (Char.chr (key - 65))
(*e: function Simple.insert_special_char *)

(*****************************************************************************)
(* Keys *)
(*****************************************************************************)
open Options

(*s: function Simple.string_to_modifier *)
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
(*e: function Simple.string_to_modifier *)
  
(*s: function Simple.modifier_to_string *)
(*e: function Simple.modifier_to_string *)
      
(*s: constant Simple.name_to_keysym *)
let name_to_keysym = 
  ("Button1", XK.xk_Pointer_Button1) ::
  ("Button2", XK.xk_Pointer_Button2) ::
  ("Button3", XK.xk_Pointer_Button3) ::
  ("Button4", XK.xk_Pointer_Button4) ::
  ("Button5", XK.xk_Pointer_Button5) ::
  XK.name_to_keysym
(*e: constant Simple.name_to_keysym *)
  
(*s: function Simple.value_to_keysym *)
(*e: function Simple.value_to_keysym *)
      
(*s: function Simple.keysym_to_value *)
(*e: function Simple.keysym_to_value *)
  
(*s: function Simple.value_to_key *)
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
(*e: function Simple.value_to_key *)
  
(*s: function Simple.key_to_value *)
let key_to_value k = Value (Keymap.print_key k)
(*e: function Simple.key_to_value *)
      
(*s: constant Simple.key_option *)
let key_option = define_option_class "Key" value_to_key key_to_value
(*e: constant Simple.key_option *)

(*s: constant Simple.binding_option *)
let binding_option = tuple2_option (smalllist_option key_option, string_option)
(*e: constant Simple.binding_option *)

(*****************************************************************************)
(* Toplevel *)
(*****************************************************************************)
  
(*s: toplevel Simple._1 *)
let _ =
  Action.define_buffer_action "overwrite_mode" (fun buf -> 
      let mode = overwrite_mode in
      if Ebuffer.modep buf mode 
      then Ebuffer.del_minor_mode buf mode
      else Ebuffer.set_minor_mode buf mode
  );

  Hook.add_start_hook (fun () ->
    let loc = Globals.location () in
    let gmap = loc.loc_map in

    (* standard chars *)
    for key = 32 to 127 do
      Keymap.add_binding gmap [NormalMap, key] self_insert_command
    done;

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
      let c_q = (ControlMap, Char.code 'q') in
      (* Keymap.add_prefix gmap [c_q]; *)
      for key = 65 to 65+25 do
        Keymap.add_binding gmap [c_q;ControlMap, key] insert_special_char;
      done;
      for key = 97 to 97+25 do
        Keymap.add_binding gmap [c_q;ControlMap, key] insert_special_char;
      done;

      Keymap.add_interactive (loc.loc_map) "fondamental_mode" 
        (fun frame -> Ebuffer.set_major_mode frame.frm_buffer 
            Ebuffer.fondamental_mode);

      Var.set_global line_comment ""
  )
(*e: toplevel Simple._1 *)
  
(*e: features/simple.ml *)
