(*s: features/color.ml *)
open Efuns

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
(* Per-mode colorization  *)
(*****************************************************************************)

(* should be set by each major mode to point to the right function *)
let color_func = Store.create_abstr "Color.color_func"

(* alt: 
 *  - recolorize after save automatically (which I do for 
 *    most pfff modes via Ebuffer.saved_buffer_hooks)
 *  - per-major-mode interactive M-x color_buffer (but I removed support
 *    for this)
 *)
let get_color_func buf =
  Var.get_var buf color_func


let color_region frame =
  let (buf, _, point) = Frame.buf_text_point frame in
  let mark = Ebuffer.get_mark buf point in
  let (start_point,end_point) =
    if point < mark then (point,mark) else (mark,point) 
  in
   (get_color_func buf) buf start_point end_point
[@@interactive]

let color_buffer_buf buf =
  let text = buf.buf_text in
  (* less: Text.unset_attrs text;   was done in ocaml_mode.mll *)

  Text.with_new_point text (fun start_point ->
  Text.with_new_point text (fun end_point ->
  Text.set_position text end_point (Text.size text);
  (* less: 
   *  let hooks = 
   *    try Var.get_global Pl_colors.color_buf_hook with Not_found ->[] in
   *  Hook.exec_hooks hooks buf;
   *)
  (get_color_func buf) buf start_point end_point;
  ))

(* alt: C-x h and then M-x color_region *)
let color_buffer frame =
  color_buffer_buf frame.frm_buffer
[@@interactive]


(*e: features/color.ml *)
