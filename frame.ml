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
open Text
open Efuns
open Ebuffer

let status_format = ref [
    StatModified , (1, 2);
    StatName, (5, 20);
    StatMode, (30,30);
    StatLine, (65, 5);
    StatCol, (70 , 5);
    StatFile, (35,15);
  ]

let status_print status str stat_type =
  status.status_modified <- true;
  try
    let (pos,maxlen) = List.assoc stat_type status.status_format in
    let len = min (String.length str) maxlen in
    String.blit str 0 status.status_string pos len;
    String.fill status.status_string (pos + len) (maxlen - len) ' '
  with
    Not_found -> ()


let status_modified frame modified =
  let status = frame.frm_status in
  if status.stat_modified <> modified then
    begin
      status_print status (if modified then "**" else "--") StatModified;
      status.stat_modified <- modified
    end

let status_col frame col =
  let status = frame.frm_status in
  if status.stat_col <> col then
    begin
      status.stat_col <- col;
      status_print status (Printf.sprintf "C%d" (col+1)) StatCol
    end

let rec print_list list =
  match list with
    [] -> ""
  | [ele] -> ele
  | ele :: ( (_ :: _) as tail) ->
      ele ^ " " ^ (print_list tail)

let status_major_mode frame  =
  let buf = frame.frm_buffer in
  let status = frame.frm_status in
  if not (status.stat_modes == buf.buf_minor_modes &&
      status.stat_mode == buf.buf_major_mode
    ) then
    begin
      status.stat_modes <- buf.buf_minor_modes;
      status.stat_mode <- buf.buf_major_mode;
      status_print status (Printf.sprintf "(%s)" 
          (print_list
            (buf.buf_major_mode.maj_name ::
            (List.map (fun m -> m.min_name) status.stat_modes))))
      StatMode;
    end

let status_line frame line =
  let status = frame.frm_status in
  if status.stat_line <> line then
    begin
      status.stat_line <- line;
      status_print status (Printf.sprintf "L%d" (line+1)) StatLine
    end

let status_name frame name =
  let status = frame.frm_status in
  if status.stat_name <> name then
    begin
      status.stat_name <- name;
      status_print status name StatName
    end

let status_file frame name =
  let status = frame.frm_status in
  if status.stat_file <> name then
    begin
      status.stat_file <- name;
      status_print status name StatFile
    end

let kill frame = 
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  frame.frm_killed <- true;
  buf.buf_shared <- buf.buf_shared - 1;
  Text.remove_point text buf.buf_point;
  Text.remove_point text buf.buf_start;
  buf.buf_point <- frame.frm_point;
  buf.buf_start <- frame.frm_start

let kill_all window =
  Window.iter kill window

let install window frame =
  if window.win_mini = (frame.frm_mini_buffer = None) then
    (kill frame; failwith "Cannot install in minibuffer");
  Window.iter (fun f -> if not(f == frame) then kill f) window;
  window.win_down <- WFrame frame;
  frame.frm_xpos <- window.win_xpos;
  frame.frm_ypos <- window.win_ypos;
  frame.frm_width <- window.win_width;
  frame.frm_height <- window.win_height;
  frame.frm_window <- window;
  if frame.frm_cutline < max_int then
    frame.frm_cutline <- window.win_width - 1;
  frame.frm_table <- 
    (Array.init window.win_height (fun i -> 
        {
          repr_line = dummy_line;
          repr_y = 0;
          repr_x = 0;
          repr_prev_reprs = [];
          repr_prev_offset = 0;
          repr_offset = 0;
          repr_reprs = [];
        } ));
  frame.frm_redraw <- true

let resize frame =
  let window = frame.frm_window in
  install window frame

let editname = "Efuns:"
let dummy_mode = Ebuffer.new_major_mode "" []
  
let create_without_top location window mini buf =
  let width = window.win_width in
  let height = window.win_height in
  let frm_start = Text.dup_point buf.buf_text buf.buf_start in
  let frm_end = Text.dup_point buf.buf_text buf.buf_start in
  let point = Text.dup_point buf.buf_text buf.buf_point in
  buf.buf_shared <- buf.buf_shared + 1;
  let status = {
      status_string = String.make 256 '-';
      status_modified = true;
      status_format = !status_format;
      stat_col = -1;
      stat_name = "";
      stat_file = "";
      stat_line = -1;
      stat_modified = (buf.buf_last_saved = version buf.buf_text);
      stat_modes = [];
      stat_mode = dummy_mode;
    } in
  String.blit editname 0 status.status_string 5 (String.length editname);
  let rec frame =
    { frm_buffer = buf;
      frm_location = location;
      frm_window = window;
      frm_last_text_updated = 0;
      frm_last_buf_updated = 0;
      
      frm_prefix = [];
      
      frm_repeat_action = 0;
      frm_last_action = Keymap.dummy_action;
      
      frm_start = frm_start;
      frm_end = frm_end;
      frm_y_offset = 0;
      frm_point = point;
      frm_cursor_x = 0;
      frm_cursor_y = 0;
      frm_cursor = String.make 1 ' ';
      frm_cursor_attr = Text.direct_attr;
      
      frm_force_point = true;
      frm_force_start = false;
      frm_force_cursor = false;
      
      frm_x_offset = 0;
      frm_cutline = width - 1;
      
      frm_has_scrollbar = 0;
      frm_has_status_line = 1;
      frm_status = status;
      
      frm_xpos = window.win_xpos;
      frm_ypos = window.win_ypos;
      frm_width = width;
      frm_height = height;
      frm_table = [||];
      frm_killed = false;
      frm_mini_buffer = mini;
      frm_redraw = true;
    } 
  in
  status_name frame buf.buf_name;
  status_major_mode frame;
  install window frame;
  frame

let active frame =
  let top_window = Window.top frame.frm_window in
  top_window.top_active_frame <- frame;
  match frame.frm_buffer.buf_filename with
    None -> ()
  | Some filename -> 
      frame.frm_location.loc_dirname <- Filename.dirname filename
      
      
let create window mini buf =
  let top_window = Window.top window in
  let frame = create_without_top top_window.top_location window mini buf in
  top_window.top_active_frame <- frame;
  frame

let create_inactive window buf =
  let top_window = Window.top window in
  let frame = create_without_top top_window.top_location window None buf in
  frame


let point_to_cursor buf point =
  let text = buf.buf_text in
  let line = Ebuffer.compute_representation buf (point_line text point) in
  let xpos = point_col text point in
  let rec iter reprs =
    match reprs with
      [] -> 0
    | repr :: tail ->
        if repr.repr_line_pos > xpos then
          iter tail
        else
          repr.repr_pos + repr.repr_charsize * (xpos - repr.repr_line_pos)
  in
  iter line.representation

let cursor_to_point frame x y =
  if (y < 0) || (x<0) ||
    (y >= frame.frm_height-1) || (x>frame.frm_cutline) then 
    raise Not_found
  else
  let line_repr = frame.frm_table.(y) in
  let y = line_repr.repr_y in
  let rec iter x reprs default =
    match reprs with
      [] -> default
    | repr :: tail -> 
        if repr.repr_size > x then
          repr.repr_line_pos + x / repr.repr_charsize
        else
          iter (x - repr.repr_size) tail 
            (repr.repr_line_pos + repr.repr_line_len)
  in
  let x = iter (x+frame.frm_x_offset+line_repr.repr_offset) 
    line_repr.repr_reprs 0 in
  x , y


let update_line top_window frame repr_string y = 
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let line_repr = frame.frm_table.(y) in
  let xterm = match top_window.top_xterm with
      None -> raise Not_found
    | Some xterm -> xterm 
  in
  let rec iter x offset reprs =
    if frame.frm_width > x then
      match reprs with
        [] -> 
          WX_xterm.clear_eol xterm 
            (x+frame.frm_xpos) (y+frame.frm_ypos)
          (frame.frm_width - x)
      | repr :: tail ->
          let len = min (frame.frm_width-x) (repr.repr_size - offset)
          in
          WX_xterm.draw_string xterm
            (x+frame.frm_xpos) (y+frame.frm_ypos)
          repr_string (repr.repr_pos+offset) len
            repr.repr_attr;
          iter (x+len) 0 tail
    else
      WX_xterm.draw_string xterm
        (frame.frm_width+frame.frm_xpos-1) (y+frame.frm_ypos)
      "/" 0 1 Text.direct_attr
  in
  iter 0 (line_repr.repr_offset+frame.frm_x_offset) line_repr.repr_reprs

let set_cursor frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let x = point_to_cursor buf point in
  let line = Ebuffer.compute_representation buf (point_line text point) in
  
  try
    for i = 0 to frame.frm_height - 1 do
      let line_repr = frame.frm_table.(i) in
      if line_repr.repr_line == line then
        let x,y =
          if x = 0 then 0,i
          else
            ((x-1) mod frame.frm_cutline) + 1, i + (x-1) / frame.frm_cutline
        in
        frame.frm_cursor_x <- x;
        frame.frm_cursor_y <- y;
        raise Exit
    done;      
    (* insert cursor is not on frame *)
    frame.frm_cursor.[0] <- '\000'
    
  with
    Exit -> 
      let rec iter reprs =
        match reprs with
          [] -> 
            frame.frm_cursor.[0] <- ' '
        | repr :: tail ->
            let point_x = point_col text point in
            if repr.repr_line_pos <= point_x &&
              repr.repr_line_pos + repr.repr_line_len > point_x then
              let pos =
                repr.repr_pos + repr.repr_charsize * 
                (point_x - repr.repr_line_pos)
              in
              frame.frm_cursor.[0] <- line.repr_string.[pos];
              frame.frm_cursor_attr <- repr.repr_attr;
            else
              iter tail
      in
      let repr_line = frame.frm_table.(frame.frm_cursor_y)
      in
      iter repr_line.repr_reprs

let update_table top_window frame =
  let buf =  frame.frm_buffer in
  let text = buf.buf_text in
  let start = frame.frm_start in
  let point = frame.frm_point in
  let width = frame.frm_width - frame.frm_has_scrollbar in
  let height = frame.frm_height - frame.frm_has_status_line in
(* assert frame.frm_y_offset >= 0 *)
  let current_n = ref (point_line text start) in
  let current_line = ref (compute_representation buf !current_n) in
  while frame.frm_y_offset < 0 && !current_n > 0 do
    current_n := !current_n - 1;
    current_line := compute_representation buf !current_n;
    let lines = !current_line.repr_len / frame.frm_cutline in
    frame.frm_y_offset <- frame.frm_y_offset + lines + 1;
  done;
  if !current_n = 0 && frame.frm_y_offset <0 then
    frame.frm_y_offset <- 0;
(* assert current_line is the first line *)
  while frame.frm_y_offset > !current_line.repr_len / frame.frm_cutline
      && !current_n < nbre_lines text
  do
    frame.frm_y_offset <- frame.frm_y_offset - 
    (!current_line.repr_len / frame.frm_cutline) - 1;
    current_n := !current_n + 1;
    current_line := compute_representation buf !current_n;
  done;
  if !current_n = nbre_lines text && 
    frame.frm_y_offset > !current_line.repr_len / frame.frm_cutline
  then
    frame.frm_y_offset <- !current_line.repr_len / frame.frm_cutline;
(* update frame.frm_start *)
  goto_line text start !current_n; 
(* update frame representation *)
  let rec iter_line y n line =
    if y < height then
      let reprs = List.rev line.representation in
      if y >= 0 then
        begin
          let line_repr = frame.frm_table.(y) in
          line_repr.repr_line <- line;
          line_repr.repr_y <- n;
          line_repr.repr_x <- 0;
          line_repr.repr_offset <- 0;
          line_repr.repr_reprs <- reprs;
        end;
      iter_repr frame.frm_cutline (y+1) n line reprs
    else
      goto_line text frame.frm_end (n-1)
  
  and iter_repr x y n line reprs =
    if x < line.repr_len then
      match reprs with
      | repr :: tail ->
          if repr.repr_pos <= x && 
            repr.repr_pos + repr.repr_size > x then
            if y = height then
              goto_line text frame.frm_end n 
            else
              begin
                if y>= 0 then
                  begin
                    let line_repr = frame.frm_table.(y) in
                    line_repr.repr_line <- line;
                    line_repr.repr_y <- n;
                    line_repr.repr_x <- repr.repr_pos;
                    line_repr.repr_offset <- x - repr.repr_pos;
                    line_repr.repr_reprs <- reprs;
                  end;
                iter_repr (x+frame.frm_cutline) (y+1) n line reprs
              end
          else
            iter_repr x y n line tail
      | _ -> 
          let line = compute_representation buf (n + 1) in
          iter_line (y+1) (n+1) line
    else  
    let line = compute_representation buf (n + 1) in
    iter_line y (n+1) line
  in
  iter_line (- frame.frm_y_offset) !current_n !current_line


let update top_window frame =
  let buf =  frame.frm_buffer in
  let text = buf.buf_text in
  let start = frame.frm_start in
  let point = frame.frm_point in
  let width = frame.frm_width - frame.frm_has_scrollbar in
  let height = frame.frm_height - frame.frm_has_status_line in
  if  buf.buf_sync && buf.buf_modified <> frame.frm_last_buf_updated then
    Text.set_position text point (Text.size text); 
  if
    (frame.frm_end < point)  || 
    (start > point) ||
    (version text <> frame.frm_last_text_updated) ||
    (buf.buf_modified <> frame.frm_last_buf_updated) ||
    frame.frm_redraw
  then
    begin
      let start_c = point_to_cursor buf start in
      if start_c > 0 then
        begin
          frame.frm_y_offset <- 
            frame.frm_y_offset - start_c / frame.frm_cutline;
          let _ = Text.bmove text start start_c in
          ()
        end;
      let point_c = point_to_cursor buf point in
      if point_c < frame.frm_x_offset then
        begin
          frame.frm_x_offset <- max (point_c - width / 2) 0;
          frame.frm_redraw <- true;
        end
      else
      if frame.frm_cutline = max_int && (point_c mod frame.frm_cutline >= frame.frm_x_offset + width - 3)  then
        begin
          frame.frm_x_offset <- point_c - (width / 2);
          frame.frm_redraw <- true;
        end;
      update_table top_window frame;
      begin
        if (frame.frm_end < point)  || (start > point)
        then
          begin
            if frame.frm_force_start then
              let x,y = 
                cursor_to_point frame frame.frm_cursor_x frame.frm_cursor_y
              in
              goto_line text frame.frm_point y;
              let _ = Text.fmove text frame.frm_point x in
              ()
            else
              begin
                goto_point text start point;
                frame.frm_y_offset <- - height / 2;
                let start_c = point_to_cursor buf start in
                if start_c > 0 then
                  begin
                    frame.frm_y_offset <- 
                      frame.frm_y_offset - start_c / frame.frm_cutline;
                    let _ = Text.bmove text start start_c in
                    ()
                  end;
                update_table top_window frame;
              end;
          end
      end;
      if frame == top_window.top_active_frame then
        begin
          frame.frm_force_start <- true; (* AVOID CYCLING IN SCROLLBAR *)
          let pos_start = get_position text frame.frm_start in
          let pos_end = get_position text frame.frm_end in
          top_window.top_scrollbar#set_params pos_start (pos_end - pos_start)
          (size text);
        end;
      frame.frm_last_text_updated <- version text;
      frame.frm_last_buf_updated <- buf.buf_modified;
      frame.frm_force_start <- false;
      for y = 0 to height - 1 do
        let line = frame.frm_table.(y) in
        if not ((line.repr_prev_reprs == line.repr_reprs) &&
            (line.repr_prev_offset == line.repr_offset)) 
          || frame.frm_redraw
        then
          begin
            line.repr_prev_reprs <- line.repr_reprs;
            line.repr_prev_offset <- line.repr_offset;
            update_line top_window frame line.repr_line.repr_string y;
          end;
      done;
      frame.frm_redraw <- false
    end;
  let xterm = match top_window.top_xterm with
      None -> raise Not_found
    | Some xterm -> xterm 
  in
  match frame.frm_mini_buffer with
    None -> 
      let status = frame.frm_status in
      status_line frame (point_line text frame.frm_point);
      status_col frame (point_col text frame.frm_point);
      status_modified frame (version text <> buf.buf_last_saved);
      status_name frame buf.buf_name;
      status_major_mode frame;
      if status.status_modified then
        WX_xterm.draw_string xterm frame.frm_xpos
          (frame.frm_ypos + frame.frm_height - 1) 
        status.status_string 0 width Text.inverse_attr
  | Some request ->
      WX_xterm.draw_string xterm 0 (top_window.top_height-1) 
      request 0 (String.length request) Text.direct_attr

exception BufferKilled
let unkill window frame =
  let buf = frame.frm_buffer  in
  if buf.buf_shared < 0 then raise BufferKilled;
  let text = buf.buf_text in
  install window frame;
  frame.frm_start <- Text.dup_point text buf.buf_start;
  frame.frm_end <- Text.dup_point text buf.buf_start;
  frame.frm_point <- Text.dup_point text buf.buf_point;
  frame.frm_y_offset <- 0;
  buf.buf_shared <- buf.buf_shared + 1;
  frame.frm_killed <- false

let move_point frame point x y =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let x, y = cursor_to_point frame (x - frame.frm_xpos) (y - frame.frm_ypos) in
  goto_line text point y;
  let _ = fmove text point x in
  ()

let current_dir frame =
  let buf = frame.frm_buffer in
  match buf.buf_filename with
    Some filename -> Filename.dirname filename ^ "/"
  | None -> buf.buf_location.loc_dirname ^ "/"


exception FoundFrame of frame

let find_buffer_frame location buf =
  try
    List.iter (fun top_window ->
        Window.iter (fun frame -> 
            if frame.frm_buffer == buf then raise (FoundFrame frame))
        top_window.top_windows
    ) location.loc_windows;
    raise Not_found
  with
    FoundFrame frame -> frame

let change_buffer_hooks = define_option ["change_buffer_hooks"] "" 
    (list_option string_option)
  [ "check_file" ]

let rec exec_named_hooks hooks frame =
  match hooks with
    [] -> ()
  | action :: hooks ->
      exec_named_hooks hooks frame;
      try execute_action action frame with _ -> ()

let rec exec_named_hooks_with_abort hooks frame =
  match hooks with
    [] -> ()
  | action :: hooks ->
      exec_named_hooks_with_abort hooks frame;
      execute_action action frame

let load_file window filename =
  let top_window = Window.top window in
  let location = top_window.top_location in
  let buf = Ebuffer.read location filename (Keymap.create ()) in
  let frame = create window None buf 
  in
  exec_named_hooks !!change_buffer_hooks frame;
  status_name frame buf.buf_name;
  frame

  
let change_buffer window name = 
  let top_window = Window.top window in
  let location = top_window.top_location in
  try
    let buf = Hashtbl.find location.loc_buffers name in
    let frame = create window None  buf 
    in
    exec_named_hooks !!change_buffer_hooks frame;
    status_name frame buf.buf_name
  with
    Not_found -> ()

let save_buffer frame =
  Ebuffer.save frame.frm_buffer

let bindings_help frame =
  let window = frame.frm_window in
  change_buffer window "*bindings*"
  