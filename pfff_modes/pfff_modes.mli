
type ('ast, 'token) for_helper = {
  parse: Common.filename -> ('ast * 'token list) list;
  highlight: tag_hook:(Parse_info.info -> Highlight_code.category -> unit) ->
             Highlight_code.highlighter_preferences -> 'ast * 'token list ->
             unit;
(*  info_of_tok:('token -> Parse_info.info); *)
}


(* will modify the text_attrs of the passed buffer and will
 * add an Outline_mode.outline_var buffer variable in the buffer
 *)
val colorize_and_set_outlines: 
  ('ast, 'token) for_helper -> Efuns.buffer -> Common.filename ->
  unit
