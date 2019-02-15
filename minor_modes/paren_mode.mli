
val is_paren_end : char -> bool
val is_paren_begin : char -> bool

val htmlp : bool ref

(* To activate paren matching visualization in a major mode, simply call
 *     Minor_modes.toggle_minor_buffer Paren_mode.mode buf
 * in the installation function of a mode.
 *
 * Alternatively, if you don't want all the closing characters to activate
 * paren mode, you can do in the setup part of the mode:
 *    [')';']'] |> List.iter (fun char ->
 *     Keymap.add_major_key mode [NormalMap, Char.code char] (fun frame ->
 *       Edit.self_insert_command frame;
 *       Paren_mode.highlight_paren frame;
 *     ));
 *)
val mode: Efuns.minor_mode

(* used by abbrevs_mode too *)
val highlight_paren : Efuns.frame -> unit (* not an action *)
