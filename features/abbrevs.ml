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

(*
Expansion des abbreviations: 
deux fonctions principales: 
- "dabbrev_expand": expansion des mots a partir de mots trouves dans les
     buffers (binding normap "M-/").
- "expand_sabbrev": expansion des mots a partir d'abbreviations trouvees
    dans des tables associees au buffer (variable "abbrev_table") 
    (binding normal: un caractere de fin de mot (espace,newline,..))
*)

open Efuns
open Text
open Frame
open Simple
open Interactive

let escaped s =
  let n = ref 0 in
  let len = String.length s in
  for i = 0 to len - 1 do
    let c = String.unsafe_get s i in
    match c with
      '\\' | '[' | ']' | '^' -> incr n
    | _ -> ()
  done;
  if !n = 0 then s else
  let ss = String.create (len + !n) in
  let p = ref 0 in
  for i = 0 to len - 1 do
    let c = String.unsafe_get s i in
    (match c with
        '\\' | '[' | ']' | '^' -> 
          String.unsafe_set ss !p '\\';
          incr p
      | _ -> ());
    String.unsafe_set ss !p c;
    incr p    
  done;
  ss
  
let dabbrev_buf = ref None
let dabbrev_expand frame = 
  let top_window = Window.top frame.frm_window in
  let location = top_window.top_location in
  let buf = frame.frm_buffer in
  let syntax = buf.buf_syntax_table in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let loop = ref false in
  let s, buf, pos, history =
    match !dabbrev_buf with
      Some (s, for_frame, for_position, for_len,buf, pos, history) 
      when frame == for_frame && for_position = get_position text point ->
        bmove text point for_len;
        Text.delete text point for_len;
        s, buf, pos, history
    | _ -> 
        let str = beginning_of_word buf point in
        str , buf, 
        get_position text point - String.length str, []
  in
  let truelen = String.length s in
  let s = escaped s in
  bmove text point truelen;
  let regexp = Str.regexp s in
  let rec iter curr_buf pos =
    let curr_text = curr_buf.buf_text in
    let mark = add_point curr_text in
    set_position curr_text mark pos;
    try
      let rec restart () =
        let len = Text.search_backward curr_text regexp mark in
        if curr_text == text && mark = point then
          raise Exit
        else
          if bmove_res curr_text mark 1 <> 0 then
            if syntax.(Char.code (get_char curr_text mark)) then
              (fmove curr_text mark 1; restart ())
            else
              (fmove curr_text mark 1; ())
          else
            ()
      in
      let rec first () =
        restart ();
        fmove curr_text mark truelen;
        let m = end_of_word curr_buf mark in
        if List.mem m history then
          (bmove curr_text mark truelen; first ())
        else
          begin
            fmove text point truelen;
            let _,len = Text.insert_res text point m in
            fmove text point len;
            dabbrev_buf := Some (s, frame, get_position text point, len, 
              curr_buf, get_position curr_text mark, m :: history);
            remove_point curr_text mark
          end
      in
      first ()
    with
      Not_found ->
        remove_point curr_text mark;
        let curr_buf = next_buffer location curr_buf in
        if curr_buf == buf then
          if !loop then raise Not_found
          else
            loop := true; (* to avoid infinite loop *)
        iter curr_buf (Text.size curr_buf.buf_text) 
    | Exit ->
        remove_point curr_text mark;
        fmove text point truelen;
        dabbrev_buf := None;
        raise Not_found
  in
  iter buf pos; ()

let abbrev_table = Local.create "abbrev_table" 
    (fun table ->
      let s = ref "" in 
      Hashtbl.iter (fun s1 s2 ->
          s := Printf.sprintf "%s%s --> %s\n" !s s1 s2
      ) table;
      !s
  )
  Local.no_input
  
let expand_sabbrev frame =
  try
    let point = frame.frm_point in
    let buf = frame.frm_buffer in
    let text = buf.buf_text in
    let abbrevs = get_local buf abbrev_table in
    let mark = dup_point text point in
    to_begin_of_word text point buf.buf_syntax_table;
    let str = Text.region text point mark in
    Text.remove_point text mark;
    let len = String.length str in
    try
      let repl = Hashtbl.find abbrevs str in
      Text.delete text point len;
      Text.insert text point repl;
      fmove text point (String.length repl); ()
    with
      Not_found -> fmove text point len; ()
  with
    _ -> ()

let _ =
  define_action "char_expand_abbrev" (fun frame ->
      expand_sabbrev frame; self_insert_command frame);
  define_action "dabbrev_expand" dabbrev_expand;
  ()
  