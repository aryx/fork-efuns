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

open Text
open Simple
open Efuns
open Top_window
  
  (* franc;ais strict *)
let accent frame =
  let key = !keypressed in
  let c = previous_char frame in
  try
    let key = 
      if key = XK.xk_grave then
        match c with
        | 'e' -> XK.xk_egrave
        | 'a' -> XK.xk_agrave
        | 'i' -> XK.xk_igrave 
        | 'o' -> XK.xk_ograve 
        | 'u' -> XK.xk_ugrave
        | 'A' -> XK.xk_Agrave
        | 'E' -> XK.xk_Egrave
        | 'I' -> XK.xk_Igrave
        | 'O' -> XK.xk_Ograve
        | 'U' -> XK.xk_Ugrave 
        | _ -> raise Not_found
      else 
      if key = XK.xk_apostrophe then
        match c with
        | 'e' -> XK.xk_eacute
        | 'E' -> XK.xk_Eacute
        
        | 'a' -> XK.xk_aacute
        | 'i' -> XK.xk_iacute
        | 'o' -> XK.xk_oacute
        | 'u' -> XK.xk_uacute
        | 'A' -> XK.xk_Aacute
        | 'I' -> XK.xk_Iacute
        | 'O' -> XK.xk_Oacute
        | 'U' -> XK.xk_Uacute
        
        | _ -> raise Not_found
      else
      if key = XK.xk_semicolon then
        match c with
          'c' -> XK.xk_ccedilla
        | _ -> raise Not_found
      else
      if key = XK.xk_asciicircum then
        match c with
        | 'e' -> XK.xk_ecircumflex
        | 'a' -> XK.xk_acircumflex 
        | 'i' -> XK.xk_icircumflex
        | 'o' -> XK.xk_ocircumflex
        | 'u' -> XK.xk_ucircumflex
        | 'A' -> XK.xk_Acircumflex
        | 'E' -> XK.xk_Ecircumflex
        | 'I' -> XK.xk_Icircumflex
        | 'O' -> XK.xk_Ocircumflex
        | 'U' -> XK.xk_Ucircumflex 
        | _ -> raise Not_found
      else              
      if key = XK.xk_quotedbl then
        match c with
        | 'e' -> XK.xk_ediaeresis
        | 'a' -> XK.xk_adiaeresis 
        | 'i' -> XK.xk_idiaeresis
        | 'o' -> XK.xk_odiaeresis
        | 'u' -> XK.xk_udiaeresis
        | 'A' -> XK.xk_Adiaeresis
        | 'E' -> XK.xk_Ediaeresis
        | 'I' -> XK.xk_Idiaeresis
        | 'O' -> XK.xk_Odiaeresis
        | 'U' -> XK.xk_Udiaeresis 
        | _ -> raise Not_found
      else              
      if key = XK.xk_asciitilde then
        match c with
        | 'n' -> XK.xk_ntilde
        | 'a' -> XK.xk_atilde
        | 'o' -> XK.xk_otilde
        | 'A' -> XK.xk_Atilde
        | 'N' -> XK.xk_Ntilde
        | 'O' -> XK.xk_Otilde 
        | _ -> raise Not_found
      else              
        raise Not_found
    in
    bmove frame.frm_buffer.buf_text frame.frm_point 1;
    insert_at_place frame (Char.chr key)
  with
    Not_found ->
      try
        let c = Char.code c in
        let c = 
          if key = XK.xk_grave then
            if c = XK.xk_egrave then 'e' else
            if c = XK.xk_agrave then 'a' else
            if c = XK.xk_igrave then 'i' else 
            if c = XK.xk_ograve then 'o' else 
            if c = XK.xk_ugrave then 'u' else
            if c = XK.xk_Agrave then 'A' else
            if c = XK.xk_Egrave then 'E' else
            if c = XK.xk_Igrave then 'I' else
            if c = XK.xk_Ograve then 'O' else
            if c = XK.xk_Ugrave then 'U' else 
              raise Not_found
          else 
          if key = XK.xk_apostrophe then
            if c = XK.xk_eacute then 'e' else
            if c = XK.xk_Eacute then 'E' else
            if c = XK.xk_aacute then 'a' else
            if c = XK.xk_iacute then 'i' else
            if c = XK.xk_oacute then 'o' else
            if c = XK.xk_uacute then 'u' else
            if c = XK.xk_Aacute then 'A' else
            if c = XK.xk_Iacute then 'I' else
            if c = XK.xk_Oacute then 'O' else
            if c = XK.xk_Uacute then 'U' else
              raise Not_found
          else
          if key = XK.xk_semicolon then
            if c = XK.xk_ccedilla then 'c' else
              raise Not_found
          else
          if key = XK.xk_asciicircum then
            if c = XK.xk_ecircumflex then 'e' else
            if c = XK.xk_acircumflex then 'a' else 
            if c = XK.xk_icircumflex then 'i' else
            if c = XK.xk_ocircumflex then 'o' else
            if c = XK.xk_ucircumflex then 'u' else
            if c = XK.xk_Acircumflex then 'A' else
            if c = XK.xk_Ecircumflex then 'E' else
            if c = XK.xk_Icircumflex then 'I' else
            if c = XK.xk_Ocircumflex then 'O' else
            if c = XK.xk_Ucircumflex then 'U' else 
              raise Not_found
          else              
          if key = XK.xk_quotedbl then
            if c = XK.xk_ediaeresis then 'e' else
            if c = XK.xk_adiaeresis then 'a' else 
            if c = XK.xk_idiaeresis then 'i' else
            if c = XK.xk_odiaeresis then 'o' else
            if c = XK.xk_udiaeresis then 'u' else
            if c = XK.xk_Adiaeresis then 'A' else
            if c = XK.xk_Ediaeresis then 'E' else
            if c = XK.xk_Idiaeresis then 'I' else
            if c = XK.xk_Odiaeresis then 'O' else
            if c = XK.xk_Udiaeresis then 'U' else 
              raise Not_found
          else              
          if key = XK.xk_asciitilde then
            if c = XK.xk_ntilde then 'n' else
            if c = XK.xk_atilde then 'a' else
            if c = XK.xk_otilde then 'o' else
            if c = XK.xk_Atilde then 'A' else
            if c = XK.xk_Ntilde then 'N' else
            if c = XK.xk_Otilde then 'O' else 
              raise Not_found
          else              
            raise Not_found
        in
        bmove frame.frm_buffer.buf_text frame.frm_point 1;
        insert_at_place frame c;
        insert_char frame (Char.chr key)
      with
        Not_found ->
          insert_char frame (Char.chr key)
          
          
let install buf =
  List.iter (fun key ->
      buf.buf_syntax_table.(key) <- true)
  [ XK.xk_egrave; XK.xk_agrave; XK.xk_igrave; XK.xk_ograve; XK.xk_ugrave;
    XK.xk_Agrave; XK.xk_Egrave; XK.xk_Igrave; XK.xk_Ograve; XK.xk_Ugrave;
    XK.xk_eacute; XK.xk_aacute; XK.xk_iacute; XK.xk_oacute; XK.xk_uacute;
    XK.xk_Aacute; XK.xk_Eacute; XK.xk_Iacute; XK.xk_Oacute; XK.xk_Uacute;
    XK.xk_ccedilla;
    XK.xk_ecircumflex; XK.xk_acircumflex; XK.xk_icircumflex;
    XK.xk_ocircumflex; XK.xk_ucircumflex; XK.xk_Acircumflex;
    XK.xk_Ecircumflex; XK.xk_Icircumflex; XK.xk_Ocircumflex;
    XK.xk_Ucircumflex;
    XK.xk_ediaeresis; XK.xk_adiaeresis; XK.xk_idiaeresis; XK.xk_odiaeresis;
    XK.xk_udiaeresis; XK.xk_Adiaeresis; XK.xk_Ediaeresis; XK.xk_Idiaeresis;
    XK.xk_Odiaeresis; XK.xk_Udiaeresis;
    XK.xk_ntilde; XK.xk_atilde; XK.xk_otilde;
    XK.xk_Atilde; XK.xk_Ntilde; XK.xk_Otilde; ]
  
let mode = Ebuffer.new_minor_mode  "accents" [install]

let _ = 
  List.iter
    (fun key -> 
      Keymap.add_binding mode.min_map [NormalMap, key] accent
  )
  [ XK.xk_apostrophe; XK.xk_grave; XK.xk_semicolon; XK.xk_asciicircum;
    XK.xk_quotedbl; XK.xk_asciitilde]

let _ = 
  define_buffer_action "accents_mode" 
    (fun buf -> 
      if Ebuffer.modep buf mode then begin
          Ebuffer.del_minor_mode buf mode
        end else
        Ebuffer.set_minor_mode buf mode)
    
    