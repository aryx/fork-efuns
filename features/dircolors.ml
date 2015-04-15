(* Yoann Padioleau
 *
 * Copyright (C) 2015 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* A port of my dircolors.el for efuns.
 * 
 * Original documentation:
 * 
 * ;;; dircolors.el -- provide the same facility of ls --color inside emacs
 * ;;; Goal
 * ; Try to colorize the buffers of emacs as ls --color do in a terminal
 * ; so if you try C-x b TAB or C-x C-f, you will see directories in blue
 * ; c source files in yellow, object files in gray, ....
 * ; It helps to visually find the file you want to open.
 * 
 * todo: factorize code with pfff/commons/file_type.ml ? 
 *)

(*****************************************************************************)
(* Colors *)
(*****************************************************************************)

(* less: could be some define_option *)
let dir_color = "CornflowerBlue"

let doc_color = "MediumTurquoise"
let doc_org_color = "light sea green"
let doc_doc_color = "turquoise"
let doc_tex_color = "medium aquamarine"
let literate_prog_color = "gold"
let ps_color = "medium purple"
let html_color = "Plum"

let package_color = "IndianRed"
let tar_color = "OrangeRed"
let compress_color = "Sienna"
let backup_color = "Magenta"

let dos_color = "LimeGreen"

let sound_color = "LightBlue"
let img_color = "Salmon"
let movie_color = "IndianRed2"


let make_color = "khaki"
let lang_color = "yellow"
let emacs_color = "GreenYellow"
let lang_2_color = "yellow3"
let lang_interface_color = "Goldenrod"
let yacc_color = "Coral"
let objet_color = "DimGray"
let asm_color = "Tan"


(*
 "the syntax is (extension_list face), where extension can be either of the
   simple form string in which case it is interpreted as an extension
   for example \"txt\" will colorise all string that ends with .txt
   or can be of the form (r regexp)"
*)

type ext =
  | E of string
  | Reg of string

let extensions = [
  ([E "txt"; E "man"; Reg "README"; Reg "readme"] , doc_color);
  ([E "doc"]                                      , doc_doc_color);
  ([E "tex"; E "texi"]                            , doc_tex_color);
  ([E "nw"]                                       , literate_prog_color);
  (* ; put before other cases *)
  ([Reg ".md5sum_"]                                 , objet_color);
  ([E "org"]                                        , doc_org_color);
  ([E "htm"; E "html"; E "html\\.gz"; E "htm\\.gz"] , html_color);


  ([E "rpm"; E "deb"; E "dmg"]                            , package_color);
  ([E "tar"; E "tgz"; E "tar.gz"; E "tar.bz2"; E "zip"; E "rar"] , tar_color);
  ([E "bak"; E "BAK"; Reg "\\.save"]       , backup_color);

  ([E"mp3"; E "s3m"; E "mod"; E "au"; E "wav"], sound_color);
  ([E "jpg"; E "gif"; E "bmp"; E "xbm"; E "tif"; E "xpm"; E "jpeg"; E "png"],
   img_color);
  ([E "avi"; E "wmv"; E "mpg"; E "mpeg"; E "mov"; E "ram"; E "mp4"; E "divx"],
   movie_color);

  ([E "ps"; E "pdf"; E "ps\\.gz"; E "eps"] , ps_color);
  ([E "cmd"; E "exe"; E "com"; E "bat"]                          , dos_color);

  ([Reg "akefile"]                         , make_color);
  ([E "ml"; E "mlx";
	E "hs"; E "lhs";
	E "scm"; E "sc";
	E "pm"; E "pl"; E "m"; E "pmi"; E "pmx";
	E "bet"
   ], lang_color);
  ([E "php";
	E "p"; E "pas";
	E "c"; E "cpp"; E "c\\+\\+"; E "cc"
   ], lang_2_color);

  ([E "el"; E "emacs"], emacs_color);
  ([E "mli";
	E "h"; E "hpp"; E "hh";
   ], lang_interface_color);

  ([E "ly"; E "mly"; E "mll";
	E "l"; E "y";
	E "l\\+\\+"; E "y\\+\\+";
	E "ll"; E "yy";
   ], yacc_color);

  ([E "class"; E "o";
    E "cmo"; E "cmi"; E "cmx"; E "cma"; E "cmxa";
    E "annot"; E "cmt"; E "cmti";
    E "clang"; E "clang2";
   ], objet_color);

  ([E "asm"; E "s"; E "S"], asm_color);
   (* ; last because can conflict *)
  ([ E"gz"], compress_color);
]

let colorize buf = 
  Simple.color buf 
    (Str.regexp ("[a-zA-Z0-9_\\-]*/")) false
    (Text.make_attr (Attr.get_color dir_color) 1 0 false);

  (* a bit brute force, but seems fast enough *)
  extensions |> List.iter (fun (exts, color) ->
    exts |> List.iter (function
      | E ext ->
          Simple.color buf 
            (Str.regexp (spf "\\b[a-zA-Z0-9_-]*\\.%s\\b" ext)) false
            (Text.make_attr (Attr.get_color color) 1 0 false);
      | Reg re ->
          Simple.color buf 
            (Str.regexp (spf "\\b[a-zA-Z0-9_-]*%s[a-zA-Z0-9_-]*\\b" re)) false
            (Text.make_attr (Attr.get_color color) 1 0 false);
    )
  )


let _ = 
  Hook.add_start_hook (fun () ->
    Hook.add_hook Select.completions_buf_hook colorize;
  )
