open Options

(* font-lock-mode like faces for efuns *)

(* pad: pixel's choice originally, a long time ago (see my .emacs)
 * related: pfff/highlight_code.ml
 *)

let keyword_color = define_option ["keyword_color"] ""
    string_option "orange"
let string_color = define_option ["string_color"] ""
    string_option "green3"
let comment_color = define_option ["comment_color"] ""
    string_option "gray"
let number_color  = define_option ["number_color"] ""
    string_option "yellow3"
let punctuation_color  = define_option ["punctuation_color"] ""
    string_option "cyan"
let operator_color  = define_option ["operator_color"] ""
    string_option "gold"

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
let builtin_color = define_option ["builtin_color"] ""
    string_option "coral"

let common_functions_color = define_option ["common_functions_color"] ""
    string_option "Violet"
let syncweb_comment_color = define_option ["syncweb_comment_color"] ""
    string_option "DimGray"
let section_comment_color = define_option ["section_comment_color"] ""
    string_option "MistyRose4"

let error_color = define_option ["error_color"] ""
    string_option "red"

let color_buf_hook = Store.create_abstr "color_buf_hook"

let color_number_and_punctuation buf =
  Color.color buf 
    (Str.regexp ("\\b[0-9]+\\b")) false
      (Text.make_attr (Attr.get_color !!number_color) 1 0 false);
  Color.color buf 
    (Str.regexp ("[|;(){}\\[\\]]")) false
      (Text.make_attr (Attr.get_color !!punctuation_color) 1 0 false);
  ()


  

let _ =  
  Hook.add_start_hook (fun () ->
    Hook.add_hook color_buf_hook color_number_and_punctuation;

    Parameter.add_option_parameter keyword_color;
    Parameter.add_option_parameter string_color;
    Parameter.add_option_parameter comment_color;

    Parameter.add_option_parameter module_color;
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

    add_option_parameter keyword_font;
    add_option_parameter string_font;
    add_option_parameter comment_font;
    add_option_parameter upper_font;
*)
