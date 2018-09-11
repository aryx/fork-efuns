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
(* Insertion *)
(*****************************************************************************)

(*s: function [[Simple.previous_char]] *)
let previous_char frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  if Text.bmove_res text point 1 = 0 then raise Not_found;
  let c = Text.get_char text point in
  Text.fmove text point 1;
  c
(*e: function [[Simple.previous_char]] *)

(*s: function [[Simple.unset_attr]] *)
let unset_attr frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  Text.unset_attrs text
(*e: function [[Simple.unset_attr]] *)
  
(*****************************************************************************)
(* Deletion *)
(*****************************************************************************)

(*s: function [[Simple.hungry_char]] *)
let hungry_char c = 
  c = ' ' || c = '\n' || c = '\t'
(*e: function [[Simple.hungry_char]] *)

(*s: function [[Simple.hungry_electric_delete]] *)
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
(*e: function [[Simple.hungry_electric_delete]] *)


(*****************************************************************************)
(* Color helpers *)
(*****************************************************************************)

(*s: function [[Simple.color]] *)
let color buf regexp strict attr =
  let text = buf.buf_text in
  Text.with_new_point text (fun point ->
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
          Text.set_attrs text point len attr;
          Text.fmove text point len;
          ()
        end
    done
  (* at some point Text.search_forward will return Not_found *)
  with Not_found -> 
    buf.buf_modified <- buf.buf_modified + 1
  )
(*e: function [[Simple.color]] *)
let color a b c d = Common.profile_code "Simple.color" 
  (fun () -> color a b c d)

(*****************************************************************************)
(* Points *)
(*****************************************************************************)

(*s: function [[Simple.point_at_mark]] *)
let point_at_mark frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in

  let mark = Ebuffer.get_mark buf point in
  let pos = Text.get_position text point in
  Text.goto_point text point mark;
  Text.set_position text mark pos
(*e: function [[Simple.point_at_mark]] *)


(*****************************************************************************)
(* Electric *)
(*****************************************************************************)

(*s: function [[Simple.electric_insert_space]] *)
let electric_insert_space frame =
  self_insert_command frame;
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let line_len = Text.point_to_bol text point in
  if line_len > 75 then
    Text.with_dup_point text point (fun mark ->
      try
        while (backward_word buf mark;
            Text.point_to_bol text mark > 75) do () done;
        forward_word buf mark; backward_word buf mark;
        Text.insert text mark "\n"
      with Not_found -> ()
    )
(*e: function [[Simple.electric_insert_space]] *)

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
(* Toplevel *)
(*****************************************************************************)

let toggle_overwrite_mode frm =
  let buf = frm.frm_buffer in
  let mode = overwrite_mode in
  if Ebuffer.has_minor_mode buf mode 
  then Ebuffer.del_minor_mode buf mode
  else Ebuffer.set_minor_mode buf mode
  
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
