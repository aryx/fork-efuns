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

(*e: features/color.ml *)
