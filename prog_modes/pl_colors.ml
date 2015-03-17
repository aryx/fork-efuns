open Options

(* font-lock-mode like faces for efuns *)

(* pad: pixel choice originally, a long time ago, see my .emacs 
 * see also pfff/highlight_code.ml too
 *)

let keyword_color = define_option ["keyword_color"] ""
    string_option "orange"
let string_color = define_option ["string_color"] ""
    string_option "green3"
let comment_color = define_option ["comment_color"] ""
    string_option "gray"

let function_name_color = define_option ["function_name_color"] ""
    string_option "LightBlue2"
let variable_name_color = define_option ["variable_name_color"] ""
    string_option "LightBlue3"
let type_color = define_option ["type_color"] ""
    string_option "palegreen"
let module_color = define_option ["module_color"] ""
    string_option "DarkSlateGray4"
let preprocessor_color = define_option ["preprocessor_color"] ""
    string_option "coral"

let error_color = define_option ["error_color"] ""
    string_option "red"

let _ =  
  Efuns.add_start_hook (fun () ->

    Simple.add_option_parameter keyword_color;
    Simple.add_option_parameter string_color;
    Simple.add_option_parameter comment_color;

    Simple.add_option_parameter module_color;
  )


(*
let keyword_font = define_option ["keyword_font"] ""
    string_option !!font
let string_font = define_option ["string_font"] ""
    string_option !!font
let comment_font = define_option ["comment_font"] ""
    string_option !!font
let module_font = define_option ["module_font"] ""
    string_option !!font
*)

(*
    add_option_parameter keyword_font;
    add_option_parameter string_font;
    add_option_parameter comment_font;
    add_option_parameter upper_font;
*)
