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
open Xtypes
open Efuns
open Text
open Frame
open Top_window

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
  
let modifier_to_string mask = 
  let s = if mask land shiftMask = 0 then "" else "S" in
  let s = if mask land controlMask = 0 then s else "C" ^ s in
  let s = if mask land mod1Mask = 0 then s else "M" ^ s in
  let s = if mask land mod2Mask = 0 then s else "2" ^ s in
  let s = if mask land mod3Mask = 0 then s else "3" ^ s in
  let s = if mask land mod4Mask = 0 then s else "4" ^ s in
  let s = if mask land mod5Mask = 0 then s else "5" ^ s in
  s
      
let name_to_keysym = 
  ("Button1", XK.xk_Pointer_Button1) ::
  ("Button2", XK.xk_Pointer_Button2) ::
  ("Button3", XK.xk_Pointer_Button3) ::
  ("Button4", XK.xk_Pointer_Button4) ::
  ("Button5", XK.xk_Pointer_Button5) ::
  XK.name_to_keysym
  
let value_to_keysym v =
  match v with
    Value v -> List.assoc v name_to_keysym
  | _ -> raise Not_found
      
let keysym_to_value k =
  Value (List.assoc k XK.keysym_to_name)
  
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
  
let key_to_value k = Value (Keymap.print_key k)
      
let key_option = define_option_class "Key" value_to_key key_to_value

let binding_option = tuple2_option (smalllist_option key_option, string_option)

let insert_string frame str =
  let text = frame.frm_buffer.buf_text in
  let point = frame.frm_point in
  let _ = Text.insert text point str in
  let _ = Text.fmove text point (String.length str) in ()

  
let single_char = String.make 1 ' '
let insert_char frame char =
  let text = frame.frm_buffer.buf_text in
  let point = frame.frm_point in
  single_char.[0] <- char;
  let _ = Text.insert text point single_char in
  let _ = Text.fmove text point 1 in () 

let insert_return frame =
  insert_char frame '\n'

let previous_char frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  if bmove_res text point 1 = 0 then raise Not_found;
  let c = get_char text point in
  let _ = fmove text point 1 in
  c

let unset_attr frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  Text.unset_attr text
  
let insert_at_place frame char =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let c = get_char text point in
  if c = '\n' then
    insert_char frame char
  else
  let session = start_session text in
  let _ = Text.delete text point 1 in
  single_char.[0] <- char;    
  let _ = Text.insert text point single_char in
  let _ = fmove text point 1 in ()


let overwrite_mode = Ebuffer.new_minor_mode "Over" []
  
  
let self_insert_command frame =
  let char = Char.chr !keypressed in
  let buf = frame.frm_buffer in
  if Ebuffer.modep buf overwrite_mode then 
    insert_at_place frame char
  else
    insert_char frame char
    
let char_insert_command char frame =
  let buf = frame.frm_buffer in
  if Ebuffer.modep buf overwrite_mode then 
    insert_at_place frame char
  else
    insert_char frame char

let move_backward frame delta =
  let text = frame.frm_buffer.buf_text in
  Text.bmove text frame.frm_point delta

let move_forward frame delta =
  let text = frame.frm_buffer.buf_text in
  Text.fmove text frame.frm_point delta

let begin_to_point frame =
  let text = frame.frm_buffer.buf_text in
  let point = frame.frm_point in
  Text.point_to_bol text point

let point_to_end frame =
  let text = frame.frm_buffer.buf_text in
  let point = frame.frm_point in
  Text.point_to_eol text point

let line_size frame =
  (point_to_end frame) + (point_to_end frame)

let beginning_of_line frame =
  let _ = move_backward frame (begin_to_point frame) in ()

let end_of_line frame =
  let eol = point_to_end frame in
  let _ = move_forward frame eol in ()

let forward_line frame =
  let text = frame.frm_buffer.buf_text in
  let point = frame.frm_point in
  if point_line text point < nbre_lines text then
    if point_to_end frame = 0 then
      begin
        let _ = move_forward frame 1 in
        end_of_line frame; ()
      end
    else
    let old_x = begin_to_point frame in
    end_of_line frame;
    let _ = move_forward frame 1 in
    let _ = move_forward frame (min old_x (point_to_end frame)) in ()

let backward_line frame =
  let text = frame.frm_buffer.buf_text in
  let point = frame.frm_point in
  if point_line text point > 0 then
    let old_x = begin_to_point frame in
    beginning_of_line frame;
    let _ = move_backward frame 1 in
    beginning_of_line frame;
    let _ = move_forward  frame (min old_x (point_to_end frame)) in ()


let kill_size = ref 0
let kill_max = 10
let kill_ring = Array.create kill_max ""
let last_kill = ref None
let last_insert = ref None

let kill_string str =
  Array.blit kill_ring 0 kill_ring 1 (kill_max - 1);
  incr kill_size;
  kill_ring.(0) <- str


let kill_text text point len =
  let point,str = Text.delete_res text point len in
  match !last_kill with
  | Some (oldtext,oldpoint) when
    oldpoint = point && oldtext == text ->
      kill_ring.(0) <- kill_ring.(0)^str
  | _ ->
      last_kill := Some (text,point);
      kill_string str

let kill_end_of_line frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let eol = point_to_end frame in
  let len =
    if eol = 0 then 1 else eol
  in
  kill_text text frame.frm_point len

let kill_eol buf point =
  let text = buf.buf_text in
  let eol = point_to_eol text point in
  let len =
    if eol = 0 then 1 else eol
  in
  kill_text text point len

let kill_bol buf point =
  let text = buf.buf_text in
  let len = point_to_bol text point in
  if len > 0 then
    ( bmove text point len;
      kill_text text point len)

let insert_killed frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let pos, len =  Text.insert_res text point kill_ring.(0) in
  fmove text point len; 
  last_insert := Some(frame,pos,0,len)

let insert_next_killed frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  match !last_insert with
  |  Some (oldframe,oldpoint,n,len) when 
    oldframe == frame && oldpoint + len = get_position text point ->
      let n = if n = (min !kill_size kill_max) - 1 then 0 else n+1 in
      bmove text point len;
      let _ = Text.delete text point len in
      let pos, len =  Text.insert_res text point kill_ring.(n) in
      fmove text point len;
      last_insert := Some(frame,pos,n,len)
  | _ -> ()

let format_to frame =
  let point = frame.frm_point in
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  Format.set_formatter_output_functions 
    (fun str pos len ->
      let s = String.sub str pos len in
      let _ = Text.insert text point s in
      Text.fmove text point len)
  (fun () -> ())

let format_to_string () =
  let string = ref "" in
  Format.set_formatter_output_functions 
    (fun str pos len ->
      let s = String.sub str pos len in
      string := !string ^ s)
  (fun () -> ());
  string


let in_next_word text mark syntax =
  while (not syntax.(Char.code (get_char text mark))) &&
    fmove_res text mark 1 = 1 do () done

let in_prev_word text mark syntax =
  while bmove_res text mark 1 = 1 &&
    (not syntax.(Char.code (get_char text mark)))
  do () done


let to_begin_of_word text mark syntax =
  if bmove_res text mark 1 = 1 then
    begin
      while syntax.(Char.code (Text.get_char text mark)) &&
        (bmove_res text mark 1) <> 0 do ()
      done;
      if not syntax.(Char.code (Text.get_char text mark)) then
        (fmove text mark 1)
    end

let to_end_of_word text mark syntax =
  while syntax.(Char.code (get_char text mark)) &&
    (fmove_res text mark 1) <> 0 do ()
  done

let to_frame f frame =
  f frame.frm_buffer frame.frm_point

let backward_word buf point =
  let text = buf.buf_text in
  let syntax = buf.buf_syntax_table in
  in_prev_word text point syntax;
  to_begin_of_word text point syntax

let forward_word buf point =
  let text = buf.buf_text in
  let syntax = buf.buf_syntax_table in
  in_next_word text point syntax;
  to_end_of_word text point syntax

let beginning_of_word buf point =
  let text = buf.buf_text in
  let syntax = buf.buf_syntax_table in
  let mark = dup_point text point in
  to_begin_of_word text mark syntax;
  let s = Text.region text mark point in
  Text.remove_point text mark;
  s

let end_of_word  buf point =
  let text = buf.buf_text in
  let syntax = buf.buf_syntax_table in
  let mark = dup_point text point in
  to_end_of_word text mark syntax;
  let s = Text.region text point mark in
  Text.remove_point text mark;
  s

let current_word buf point =
  let text = buf.buf_text in
  let syntax = buf.buf_syntax_table in
  let start = dup_point text point in
  to_begin_of_word text start syntax;
  let term = dup_point text point in
  to_end_of_word text term syntax;
  let word = Text.region text start term in
  remove_point text start;
  remove_point text term;
  word

let current_word buf point =
  (beginning_of_word buf point) ^ (end_of_word buf point)

  
  
let dirname frame filename =
  let filename =
    if Filename.is_relative filename then
      Filename.concat (Frame.current_dir frame) filename
    else
      filename
  in
  Filename.dirname filename

let buffer_list frame =
  let top_window = Window.top frame.frm_window in
  let location = top_window.top_location in
  let list = ref [] in
  Hashtbl.iter (fun name _ -> list := name :: !list) location.loc_buffers;
  !list


let delete_char frame =
  let text = frame.frm_buffer.buf_text in
  let _ = Text.delete text frame.frm_point 1 in
  ()

let delete_backspace_char frame =
  let text = frame.frm_buffer.buf_text in
  if Text.bmove_res text frame.frm_point 1 <> 0 then
    let _ = Text.delete text frame.frm_point 1 in
      ()

let hungry_char c = 
  c = ' ' || c = '\n' || c = '\t'

let hungry_electric_delete frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let session = start_session text in
  let c1 = previous_char frame in
  delete_backspace_char frame;
  let c2 = previous_char frame in
  begin
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
      with
        Not_found -> ()
  end;
  commit_session text session
  
(*
     let move_backward frame =
   Functions.move_backward frame 1; ()

let move_forward frame = 
  Functions.move_forward frame 1; () 
*) 

let forward_screen frame =
  frame.frm_force_start <- true;
  frame.frm_redraw <- true;
  frame.frm_y_offset <- frame.frm_y_offset + frame.frm_height - 2

let backward_screen frame =
  frame.frm_force_start <- true;
  frame.frm_redraw <- true;
  frame.frm_y_offset <- frame.frm_y_offset - frame.frm_height + 2

let scroll_line frame n =
  frame.frm_force_start <- true;
  frame.frm_redraw <- true;
  frame.frm_y_offset <- frame.frm_y_offset + n

let recenter frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  frame.frm_force_start <- true;
  frame.frm_redraw <- true;
  goto_point text frame.frm_start frame.frm_point;
  frame.frm_y_offset <- - frame.frm_height/2

let end_of_file frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  Text.set_position text frame.frm_point (Text.size text)

let begin_of_file frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  Text.set_position text frame.frm_point 0

let delete_backward_word buf point =
  let text = buf.buf_text in
  let old_point = dup_point text point in
  backward_word buf point;
  let _ = Text.delete text point (Text.distance text point old_point) in
  remove_point text old_point;
  ()

let delete_forward_word buf point =
  let text = buf.buf_text in
  let old_point = dup_point text point in
  forward_word buf point;
  let len = Text.distance text old_point point in
  remove_point text old_point;
  Text.bmove text point len;
  let _ = Text.delete text point len in
  ()

let undo frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in  
  let point = frame.frm_point in
  let action, at_point, len = Text.undo text in
  frame.frm_last_text_updated <- version text - 1;
  Text.set_position text point at_point;
  Text.fmove text point len; ()

let kill_region frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let mark =
    match buf.buf_mark with
      None -> failwith "No mark set"
    | Some mark -> mark
  in
  let (start,term) = 
    if mark > point then (point,mark) else (mark,point)
  in
  let _,region = Text.delete_res text start (distance text start term) in
  kill_string region


let mouse_set_frame frame =
  let top_window = Window.top frame.frm_window in
  let frame = mouse_set_active top_window in
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let mark = Ebuffer.get_mark buf point in
  goto_point text mark point;
  ()
 
(* hightlighting of regions *)  
let highlighted = ref None
let highlight_bit = 1 lsl 24

let unhightlight_region buf debut fin =
  let text = buf.buf_text in
  let curseur = add_point text in
  let final = add_point text in
  set_position text curseur debut;
  set_position text final fin;
  let unhigh = lnot (1 lsl 24) in
  while curseur < final do
    let attr = get_attr text curseur in
    set_char_attr text curseur (attr land unhigh);
    fmove text curseur 1;
  done;
  remove_point text curseur;
  remove_point text final;
  buf.buf_modified <- buf.buf_modified + 1

let hightlight_region buf debut fin =
  let text = buf.buf_text in
  let curseur = add_point text in
  let final = add_point text in
  set_position text curseur debut;
  set_position text final fin;
  let high = 1 lsl 24 in
  while curseur < final do
    let attr = get_attr text curseur in
    set_char_attr text curseur (attr lor high);
    fmove text curseur 1
  done;
  remove_point text curseur;
  remove_point text final;
  buf.buf_modified <- buf.buf_modified + 1

let highlighted_chars = ref []

let unhightlight location =
  List.iter (fun (buf,curseur,attr) ->
      let text = buf.buf_text in
      set_char_attr text curseur attr;
      buf.buf_modified <- buf.buf_modified + 1;
      remove_point text curseur
  ) !highlighted_chars;
  highlighted_chars := [];
    match !highlighted with
      None -> ()
    | Some (frame,debut,fin) -> 
        if !keypressed <> XK.xk_Pointer_Drag1 then
          let top_window = Window.top frame.frm_window in
          let xterm = Window.xterm top_window in
          highlighted := None;
          
          let buf = frame.frm_buffer in
          let text = buf.buf_text in
          let curseur = add_point text in
          let final = add_point text in
          set_position text curseur debut;
          set_position text final fin;
          let str = Text.region text curseur final in
          remove_point text curseur;
          remove_point text final;
          kill_string str;
          WX_xterm.set_cutbuffer xterm str;
          unhightlight_region buf debut fin
  
let highlight frame =
  let frame =
    match !highlighted with
      None -> frame
    | Some (frame,d,f) -> frame
  in    
  let buf = frame.frm_buffer in
  let point = frame.frm_point in
  let text = buf.buf_text in
  let mark = Ebuffer.get_mark buf point in
  let debut, fin =
    if point < mark then
      point,mark
    else
      mark,point
  in
  let pos1 = get_position text debut in
  let pos2 = get_position text fin in
  let debut,fin =
    match !highlighted with
      None -> pos1,pos2
    | Some (frame,d,f) ->
        if d < pos1 then    
          unhightlight_region buf d pos1; 
        if f > pos2 then
          unhightlight_region buf pos2 f;
        if pos1 < d then
          pos1,d
        else
        if pos2 > f then
          f, pos2
        else
          pos1,pos1
  in
  highlighted := Some (frame, pos1, pos2);
  hightlight_region buf debut fin


let htmlp = ref false
let is_paren_end c = (c == '}') || (c == ']') || (c == ')')
  ||  (!htmlp && c == '>')
let is_paren_begin c = (c == '{') || (c == '[') || (c == '(')
  ||  (!htmlp && c == '<')

let highlight_paren frame =
  htmlp := (!keypressed = Char.code '>');
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let curseur = dup_point text point in
  if bmove_res text curseur 1 = 0 then remove_point text curseur
  else
  let c = get_char text curseur in
  if not(is_paren_end c) then remove_point text curseur 
  else
  let rec iter stack =
    if bmove_res text curseur 1 = 0 then
      begin
        remove_point text curseur;
        Top_window.mini_message frame "No matching parenthesis"
      end
    else
    let d = get_char text curseur in
    if is_paren_end d then
      begin
        iter (d :: stack)
      end
    else
    if is_paren_begin d then
      match stack with
        [] -> (* found matching par *)
          let attr = get_attr text curseur in
          highlighted_chars := (buf,curseur,attr) :: !highlighted_chars;
          set_char_attr text curseur (attr lor (1 lsl 24));
          buf.buf_modified <- buf.buf_modified + 1
      | _ :: stack -> (* don't try to match *)
          iter stack
    else
      iter stack
  in
  iter []
  
  
  (* C'est tout simple. On arrive dans cette fonction quand on est en train
  de bouger la souris avec le bouton appuyer. La frame courante est donc 
  correcte. On peut utiliser la position de la souris pour trouver la 
  nouvelle position du curseur dans la frame. Si on en sort, on peut
  ou prendre la derniere position, ou la premiere.
  *)
let mouse_drag_region frame =
  let top_window = Window.top frame.frm_window in
  let point = frame.frm_point in
  begin
    try
      move_point frame point !mouse_x !mouse_y
    with
      Not_found ->
        let buf = frame.frm_buffer in
        let text = buf.buf_text in
        let y = !mouse_y - frame.frm_ypos in
        if y < 0 then
          ( scroll_line frame (y-1);
            goto_point text point frame.frm_start;
            bmove text point 1; ())
        else
        if y >= frame.frm_height - 1 then
          (
            scroll_line frame (y - frame.frm_height + 2);
            goto_point text point frame.frm_end;
            fmove text point 1; ())
  end;
  highlight frame;
  let top_window = Window.top frame.frm_window in
  let xterm = top_window.top_term in
  Selection.setSelection xterm#display xterm#window XA.xa_primary
    (fun target ->
      if target = XA.xa_string then 
        match !highlighted with
          None -> raise Not_found
        | Some (frame,debut,fin) -> 
            let buf = frame.frm_buffer in
            let text = buf.buf_text in
            let curseur = add_point text in
            let final = add_point text in
            set_position text curseur debut;
            set_position text final fin;
            let str = Text.region text curseur final in
            remove_point text curseur;
            remove_point text final;
            
            1, str
      else raise Not_found
  ) !Eloop.event_time

let  mouse_yank_at_click frame =
  let top_window = Window.top frame.frm_window in
  let frame = mouse_set_active top_window in
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let xterm = Window.xterm top_window in
  let str = WX_xterm.get_cutbuffer xterm in
  let _ = Text.insert text point str in
  Text.fmove text point (String.length str)


let mouse_save_then_kill frame =
  let top_window = Window.top frame.frm_window in
  let frame = Top_window.find_selected_frame top_window in
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let mark = Ebuffer.get_mark buf point in
  let new_point = add_point text in
  Frame.move_point frame new_point !mouse_x !mouse_y;
  if point = new_point then
    begin
      remove_point text new_point;
      let (start,term) =
        if point < mark then (point,mark) else (mark,point) 
      in
      let _ = Text.delete text start (Text.distance text start term) in
      ()
    end
  else
  let xterm = Window.xterm top_window in
  goto_point text mark point;
  goto_point text point new_point;
  remove_point text new_point;
  let str = Text.region text mark point in
  kill_string str;
  WX_xterm.set_cutbuffer xterm str;
  highlight frame

let next_buffer location buf =
  let buf_list = Utils.list_of_hash location.loc_buffers in
  let rec iter list =
    match list with
      [] -> raise Not_found 
    | (name,b) :: tail ->
        if b == buf then 
          match tail with
            [] -> snd (List.hd buf_list)
          | (_,b)::_ -> b
        else
          iter tail
  in
  iter buf_list

let kill_buffer frame =
  let window = frame.frm_window in
  let top_window = Window.top window in
  let location = top_window.top_location in
  let buf = frame.frm_buffer in
  let new_buf = next_buffer location buf in
  let new_frame = Frame.create window None new_buf in
  if buf.buf_shared = 0 then Ebuffer.kill location buf

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

let point_at_mark frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let mark = Ebuffer.get_mark buf point in
  let point_pos = get_position text point in
  goto_point text point mark;
  set_position text mark point_pos


let on_word buf point f =
  let text = buf.buf_text in
  let session = start_session text in
  let syntax = buf.buf_syntax_table in
  to_begin_of_word text point syntax;
  let pos1 = dup_point text point in
  to_end_of_word text point syntax;
  let _,word1 = Text.delete_res text pos1 (distance text pos1 point) in
  let w = f word1 in
  let _ = Text.insert text pos1 w in
  fmove text point (String.length w);
  commit_session text session;
  remove_point text pos1
  
let transpose_words buf point =
  let text = buf.buf_text in
  let session = start_session text in
  let syntax = buf.buf_syntax_table in
  in_prev_word text point syntax;
  to_begin_of_word text point syntax;
  let pos1 = dup_point text point in
  to_end_of_word text point syntax;
  let _,word1 = Text.delete_res text pos1 (distance text pos1 point) in
  goto_point text point pos1;
  in_next_word text point syntax;
  let pos2 = dup_point text point in
  to_end_of_word text point syntax;
  let _,word2 = Text.delete_res text pos2 (distance text pos2 point) in    
  let _ = Text.insert text pos1 word2 in
  let _ = Text.insert text pos2 word1 in
  fmove text point (String.length word1);
  commit_session text session;
  remove_point text pos1;
  remove_point text pos2


let transpose_chars buf point =
  let text = buf.buf_text in
  let session = start_session text in
  bmove text point 1;
  let pos,c1 = Text.delete_res text point 1 in
  fmove text point 1;
  let _ = Text.insert text point c1 in
  commit_session text session;
  fmove text point 1;
  ()


let backward_paragraph buf point =
  let text = buf.buf_text in
  while bmove_res text point 1 = 1 && 
    (let c = get_char text point
      in
      c = '\n' || c = ' ') 
  do () done;
  try
    let _ = Text.search_backward text (Str.regexp "\n *\n") point in
    fmove text point 1; ()
  with
    Not_found ->
      set_position text point 0

let forward_paragraph buf point =
  let text = buf.buf_text in
  while
    (let c = get_char text point
      in
      c = '\n' || c = ' ') 
    && fmove_res text point 1 = 1 do () done;
  try
    let _ = Text.search_forward text (Str.regexp "\n *\n") point in
    fmove text point 1; ()
  with
    Not_found -> 
      set_position text point (Text.size text)


let electric_insert_space frame =
  self_insert_command frame;
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let line_len = point_to_bol text point in
  if line_len > 75 then
    let mark = dup_point text point in
    (try
        while (backward_word buf mark;
            point_to_bol text mark > 75) do () done;
        forward_word buf mark; backward_word buf mark;
        let _ = Text.insert text mark "\n" in ()
      with
        Not_found -> ());
    remove_point text mark

let simplify text start point =
  let start = dup_point text start in
  let rec iter last_c =
    if start < point then
      let c = get_char text start in
      if c = ' ' || c = '\n' || c = '\t' then
        ( let _ = delete text start 1 in
          iter ' ')
      else
      if last_c = ' ' then
        ( let _ = insert text start " " in
          fmove text start 2;
          iter 'a')
      else
        ( fmove text start 1;
          iter 'a')
  in
  iter 'a';
  remove_point text start

let line_comment = Local.create_abstr "Fill_mode.line_comment"

(* We will have to modify this to handle line_comment soon !! *)
let fill_paragraph frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let start = dup_point text point in
  let session = start_session text in
  let syntax = buf.buf_syntax_table in
  backward_paragraph buf start;
  let fin = dup_point text start in
  forward_paragraph buf fin;
  simplify text start fin;
  let _ = insert text start "\n" in
  let rec iter count last_space =
    if compare text start fin < 0 then
    if fmove_res text start 1 = 1 then 
      let c = get_char text start in  
        if c = ' ' then (* good, a new space *)
          iter (count+1) 0
      else
      if count > 75 && count <> last_space then 
          begin
          bmove text start (last_space+1);
          delete text start 1;
          insert text start "\n";
          fmove text start 1;
          iter 0 0
          end
        else
          iter (count+1) (last_space+1)
  in
  iter 0 0;  
  let _ = insert text fin "\n" in
  remove_point text start;
  remove_point text fin;
  commit_session text session
  
(* modify the indentation of (point) line. Does not modify point *)
let set_indent text point offset = 
  let curseur = dup_point text point in
  bmove text curseur (point_to_bol text curseur);
  let rec iter offset =
    let c = get_char text curseur in
    if offset > 0 then
      if c = ' ' then
        (fmove text curseur 1; iter (offset - 1))
      else
      if c = '\t' then
        (let _ = delete text curseur 1 in iter offset)
      else
        (Text.insert text curseur (String.make offset ' '); ())
    else
    if c = ' ' || c='\t' then
      (Text.delete text curseur 1;
        iter 0)
  in
  iter offset;
  remove_point text curseur

let insert_special_char frame =
  let key = !keypressed in
  let char = Char.chr key in
  if char >= 'a' && char <= 'z' then
    insert_char frame (Char.chr (key - 97))
  else
    insert_char frame (Char.chr (key - 65))

(* a hole is two consecutive '^' chars *)
let next_hole frame = 
  let buf = frame.frm_buffer in
  let point = frame.frm_point in
  let text = buf.buf_text in
  let curseur = dup_point text point in
  while 
    not ((get_char text curseur = '^') && (fmove_res text curseur 1 = 1) &&
      (get_char text curseur = '^')) && (fmove_res text curseur 1 = 1) do () done;
  if get_char text curseur = '^' then
    (bmove text curseur 1;
      delete text curseur 2;
      goto_point text point curseur);
  remove_point text curseur

let insert_structure s frame =
  let buf = frame.frm_buffer in
  let point = frame.frm_point in
  let text = buf.buf_text in
  insert text point s;
  next_hole frame

let install_structures buf list =
  List.iter (
    fun (key, s) ->
      Keymap.add_binding buf.buf_map key (insert_structure s)
  ) list
  
open Options

type parameter =   (string * ((string -> Obj.t) * (Obj.t -> string) * 
      Obj.t option_record))
  
let parameters_var = Local.create_abstr "parameters"
  
let add_parameter location (name : string) (input : string -> 'a) 
  (print : 'a -> string) (param : 'a option_record) =
  let (input : string -> Obj.t) = Obj.magic input in
  let (print : Obj.t -> string) = Obj.magic print in
  let (param : Obj.t option_record) = Obj.magic param in
  set_global location parameters_var (
    (name, (input, print, param)) :: 
    (try get_global location parameters_var with _ -> []))

  (*
external id : 'a -> 'a = "%identity"
let add_string_parameter location name param = 
  add_parameter location name id id param
let add_int_parameter location name param = 
  add_parameter location name int_of_string string_of_int param
let add_float_parameter location name param = 
  add_parameter location name float_of_string string_of_float param
let add_bool_parameter location name param = 
  add_parameter location name bool_of_string string_of_bool param
    *)

let add_option_parameter location option =
  add_parameter location (shortname option)
  (fun s -> from_value (get_class option) (Value s))
  (fun v -> 
      match to_value (get_class option) v with
        Value s -> s
      | _ -> failwith "Unable to print option") option
  
let all_params = ref None
let all_parameters frame _ =
  let parameters = try get_global frame.frm_location parameters_var with _ -> []
  in
  match !all_params with
    Some (f,l) when f == parameters -> l
  | _ ->
      let list = List.map fst parameters in
      all_params := Some (parameters, list);
      list

  
let _ =
  define_buffer_action "overwrite_mode" 
    (fun buf -> 
      let mode = overwrite_mode in
      if Ebuffer.modep buf mode then begin
          Ebuffer.del_minor_mode buf mode
        end else
        Ebuffer.set_minor_mode buf mode);

  Efuns.add_start_hook (fun location ->
      let gmap = location.loc_map in
      (* unhightlight region *)
      add_hook location Top_window.handle_key_start_hook unhightlight;      
      (* standard chars *)
      for key = 32 to 127 do
        Keymap.add_binding gmap [NormalMap, key] self_insert_command
      done;
      (* special for AZERTY keyboards *)
      Array.iter (fun (key, char) ->
          Keymap.add_binding gmap [NormalMap, key] (char_insert_command char)
      ) [| 
        (XK.xk_eacute, 'é');
        (XK.xk_egrave, 'è');
        (XK.xk_ccedilla, 'ç');
        (XK.xk_agrave, 'à');
        (XK.xk_ugrave, 'ù');
        (XK.xk_mu, 'µ'); 
        (XK.xk_sterling, '£');
        (XK.xk_section, '§');
        (XK.xk_degree,  '°');
        |];
      let c_q = (ControlMap, Char.code 'q') in
      (* Keymap.add_prefix gmap [c_q]; *)
      for key = 65 to 65+25 do
        Keymap.add_binding gmap [c_q;ControlMap, key] insert_special_char;
      done;
      for key = 97 to 97+25 do
        Keymap.add_binding gmap [c_q;ControlMap, key] insert_special_char;
      done;
      Keymap.add_binding gmap [NormalMap, XK.xk_Pointer_Drag1]
        mouse_drag_region;
      Keymap.add_interactive (location.loc_map) "fondamental-mode" 
        (fun frame -> Ebuffer.set_major_mode frame.frm_buffer 
            Ebuffer.fondamental_mode);
      set_global location line_comment ""
  )

  