(*s: features/abbrevs.ml *)
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


(*s: function [[Abbrevs.escaped]] *)
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
(*e: function [[Abbrevs.escaped]] *)
  
(*s: constant [[Abbrevs.dabbrev_buf]] *)
let dabbrev_buf = ref None
(*e: constant [[Abbrevs.dabbrev_buf]] *)
(*s: function [[Abbrevs.dabbrev_expand]] *)
let dabbrev_expand frame = 
  let buf = frame.frm_buffer in
  let syntax = buf.buf_syntax_table in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let loop = ref false in
  let s, buf, pos, history =
    match !dabbrev_buf with
      Some (s, for_frame, for_position, for_len,buf, pos, history) 
      when frame == for_frame && for_position = Text.get_position text point ->
        Text.bmove text point for_len;
        Text.delete text point for_len;
        s, buf, pos, history
    | _ -> 
        let str = Simple.beginning_of_word buf point in
        str , buf, 
        Text.get_position text point - String.length str, []
  in
  let truelen = String.length s in
  let s = escaped s in
  Text.bmove text point truelen;
  let regexp = Str.regexp s in
  let rec iter curr_buf pos =
    let curr_text = curr_buf.buf_text in
    Text.with_new_point curr_text (fun mark ->
      Text.set_position curr_text mark pos;
      try
        let rec restart () =
          let _len = Text.search_backward curr_text regexp mark in
          if curr_text == text && mark = point then
            raise Exit
          else
            if Text.bmove_res curr_text mark 1 <> 0 then
              if syntax.(Char.code (Text.get_char curr_text mark)) 
              then begin Text.fmove curr_text mark 1; restart () end
              else Text.fmove curr_text mark 1
            else
              ()
        in
        let rec first () =
          restart ();
          Text.fmove curr_text mark truelen;
          let m = Simple.end_of_word curr_buf mark in
          if List.mem m history then begin
            Text.bmove curr_text mark truelen; 
            first ()
          end else begin
            Text.fmove text point truelen;
            let _,len = Text.insert_res text point m in
            Text.fmove text point len;
            dabbrev_buf := Some (s, frame, Text.get_position text point, len, 
              curr_buf, Text.get_position curr_text mark, m :: history);
          end
        in
        first ()
      with 
      | Not_found ->
          let curr_buf = Multi_buffers.next_buffer curr_buf in
          if curr_buf == buf 
          then
            if !loop 
            then raise Not_found
            else loop := true (* to avoid infinite loop *)
          ;
          iter curr_buf (Text.size curr_buf.buf_text) 
      | Exit ->
          Text.fmove text point truelen;
          dabbrev_buf := None;
          raise Not_found
     )
  in
  iter buf pos; ()
(*e: function [[Abbrevs.dabbrev_expand]] *)

(*s: constant [[Abbrevs.abbrev_table]] *)
let abbrev_table = Store.create "abbrev_table" 
    (fun table ->
      let s = ref "" in 
      Hashtbl.iter (fun s1 s2 ->
          s := Printf.sprintf "%s%s --> %s\n" !s s1 s2
      ) table;
      !s
  )
  Store.no_input
(*e: constant [[Abbrevs.abbrev_table]] *)
  
(*s: function [[Abbrevs.expand_sabbrev]] *)
let expand_sabbrev frame =
  try
    let point = frame.frm_point in
    let buf = frame.frm_buffer in
    let text = buf.buf_text in
    let abbrevs = Var.get_local buf abbrev_table in
    let str =
      Text.with_dup_point text point (fun mark ->
        Simple.to_begin_of_word text point buf.buf_syntax_table;
        Text.region text point mark 
      )
    in
    let len = String.length str in
    try
      let repl = Hashtbl.find abbrevs str in
      Text.delete text point len;
      Text.insert text point repl;
      Text.fmove text point (String.length repl); ()
    with
      Not_found -> Text.fmove text point len; ()
  with _ -> ()
(*e: function [[Abbrevs.expand_sabbrev]] *)

(*s: toplevel [[Abbrevs._1]] *)
let _ =
  Action.define_action "char_expand_abbrev" (fun frame ->
      expand_sabbrev frame; 
      Simple.self_insert_command frame
  );
  Action.define_action "dabbrev_expand" dabbrev_expand;
  ()
(*e: toplevel [[Abbrevs._1]] *)
  
(*e: features/abbrevs.ml *)
