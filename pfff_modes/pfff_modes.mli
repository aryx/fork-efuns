
type ('ast, 'token) for_helper = {
  parse: Common.filename -> ('ast * 'token list) list;

  (* note: there is no guarantee in which order the hooks are called as
   * some pfff modes visit first the AST and then the tokens
   * in which case some hooks for early tokens might be called
   * after tokens that come later but were present in the 
   * AST and visited.
   *)
  highlight: tag_hook:(Parse_info.t -> Highlight_code.category -> unit) ->
             Highlight_code.highlighter_preferences -> Common.filename ->
             'ast * 'token list ->
             unit;
(*  info_of_tok:('token -> Parse_info.info); *)
}


(* will modify the text_attrs of the passed buffer and will
 * add an Outline_mode.outline_var buffer variable in the buffer
 *)
val colorize_and_set_outlines: 
  ('ast, 'token) for_helper -> Efuns.buffer -> Common.filename ->
  unit

(* maintain some dir -> db information *)
val load_database_code: Efuns.action
val load_graph_code: Efuns.action

val goto_def: Efuns.action

