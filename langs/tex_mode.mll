{
  open Options
  open Lexing
  open Text  
  open Compil
  
  
  type token =
    ANY
  | COMMENT
  | MATHMODE
  | STARTMATH
  | ENDMATH
  | STARTTABBING
  | ENDTABBING
  | KWDTABBING
  | STARTARRAY
  | ENDARRAY
  | KWDARRAY
  | STARTENV
  | ENDENV
  | CONDITION    
  | ITEM
  | PARAMETER
  | COMMAND
  | FONT
  | EOF
  | LBRACE
  | RBRACE
  
  let lexer_start = ref 0
  let position lexbuf =
    let b = lexeme_start lexbuf in
    let e = lexeme_end lexbuf in
    b + !lexer_start, e - b
  
  let lexing text start_point end_point =
    lexer_start := get_position text start_point;
    Text.lexing text start_point end_point

}

rule token = parse
(* comments *)
    '%'+ [^ '\n']* '\n' { position lexbuf, COMMENT }
(* Math mode *)
  | "$" | "$$" { position lexbuf, MATHMODE }
  
  | "\\(" | "\\[" { position lexbuf, STARTMATH }
  
  |  "\\)" | "\\]" { position lexbuf, ENDMATH }

    (* Tabbing *)
  | "\\begin" ' '* "{tabbing}" { position lexbuf, STARTTABBING }
  | "\\end" ' '* "{tabbing}"   { position lexbuf, ENDTABBING }
  | "\\>" | "\\="
  | "\\kill"  { position lexbuf, KWDTABBING }

(* tables and array *)
  | "\\begin" ' '* ("{tabular}" | "{array}") { position lexbuf, STARTARRAY }
  | "\\end" ' '* ("{tabular}" | "{array}")   { position lexbuf, ENDARRAY }
  | "&"                                      { position lexbuf, KWDARRAY }
      
(* environments *)
  |   "\\begin" " "* "{" ['A'-'Z' 'a'-'z']+ '*'?"}"
      { position lexbuf, STARTENV }
  |  "\\end" " " * "{" ['A'-'Z' 'a'-'z']+ '*'? "}"
      { position lexbuf, ENDENV }    
  | "\\item" { position lexbuf, ITEM }
(* Ignore font definitions ... *)
  | "\\font" "\\" ['A'-'Z' 'a'-'z']+ ' '* '='? ' '* ['a'-'z' 'A'-'Z' '0'-'9']+
      { position lexbuf, FONT }
(* conditionals *)
  | "\\newif"
  | "\\else"
  | "\\fi" { position lexbuf, CONDITION }

(* General case for commands *)
  | '#' ['1'-'9'] { position lexbuf, PARAMETER }
  | "\\" (('@' ? ['A'-'Z' 'a'-'z']+ '*'?) | [^ 'A'-'Z' 'a'-'z'])
    { position lexbuf, COMMAND }
  | '{' { position lexbuf, LBRACE }
  | '}' { position lexbuf, RBRACE }
  | eof { position lexbuf, EOF }
  | _   { position lexbuf, ANY }

{
open Text
open Efuns
open Interactive
open Simple
open Abbrevs
open Keymap
open Window

let abbreviations = define_option ["tex_mode"; "abbrevs"] ""
    (list_option string2_option) []
  
let _ = 
  if !!abbreviations = [] then
    abbreviations =:=
      [ "\s", "\section";
      "\ss", "\subsection";
      "\sss", "\subsubsection";
      "\p", "\paragraph";
      "\begit", "\begin{itemize}";
      "\endit", "\end{itemize}";
      "\startarticle", "\\documentclass[twocolumn]{article}

\\usepackage{isolatin1}
\\usepackage{francais}
\\usepackage{url}
\\usepackage{epsfig}

\\newcommand{\\comment}[1]{{\\hfuzz 4pt\\par
 \\medskip\\noindent\\hskip-\\fboxsep\\hskip-\\fboxrule\\fbox{\\parbox{\\hsize}{\\it
 #1}}\\par}}

\\newcommand{\\figurebox}[1]{#1}
\\newcommand{\\smallsf}{\\small\\sffamily}
\\newcommand{\\code}{\\texttt}
\\newcommand{\\func}{\\textsf}
\\newcommand{\\msg}{\\textsf}

\\newcommand{\\psfigure}[3]{ % {scale}{filename=label}{caption}
 \\begin{figure}[t]\\begin{center}
 \\figurebox{\\epsfig{file=figs/#2.eps,width=#1\\hsize}}
 \\begin{quote}\\let\\normalsize\\small\\caption{#3\\label{fig:#2}}\\end{quote}
 \\end{center}\\end{figure}}

\\begin{document}

\\title{^^} 

\\bibliographystyle{plain}

\\author{^^}

\\maketitle

\\abstract{^^}

^^
\\bibliography{^^}
\\end{document}
";
    ]
    
type mode =
  Math_mode
| Arg_mode
| Normal_mode

let keyword_color = define_option ["tex_mode"; "keyword_color"] ""
    string_option "red"
let string_color = define_option ["tex_mode"; "string_color"] ""
    string_option "blue"
let comment_color = define_option ["tex_mode"; "comment_color"] ""
    string_option "cadetblue"
let upper_color = define_option ["tex_mode"; "upper_color"] ""
    string_option "blue"
let keyword_font = define_option ["tex_mode"; "keyword_font"] ""
    string_option !!font
let string_font = define_option ["tex_mode"; "string_font"] ""
    string_option !!font
let comment_font = define_option ["tex_mode"; "comment_font"] ""
  string_option !!font
let upper_font = define_option ["tex_mode"; "upper_font"] ""
  string_option !!font
  
let tex_color_buffer buf =
  let location = buf.buf_location in
  let red_attr = make_attr (get_color location !!keyword_color) 1 
    (get_font location !!keyword_font) false in
  let yellow_attr = make_attr (get_color location !!string_color) 1 
    (get_font location !!string_font)    false in
  let blue_attr = make_attr (get_color location !!comment_color) 1 
    (get_font location !!comment_font) false in
  let gray_attr = make_attr (get_color location !!upper_color) 1 
    (get_font location !!upper_font)     false in
  let text = buf.buf_text in
  let start_point = Text.add_point text in
  let curseur = add_point text in
  let end_point = add_point text in 
  set_position text end_point (size text);
  let lexbuf = lexing text start_point end_point in
  let rec iter mode lexbuf =
    let (pos,len), token = token lexbuf in
    let mode =
      match token with
        EOF -> 
          raise Exit
      | COMMENT
        ->
          set_position text curseur pos;
          set_attr text curseur len blue_attr;
          mode
      | MATHMODE ->
          set_position text curseur pos;
          set_attr text curseur len yellow_attr;
          (match mode with
              Math_mode -> Normal_mode
            | _ -> Math_mode)
      | STARTMATH ->
          set_position text curseur pos;
          set_attr text curseur len yellow_attr;
          Math_mode          
      | ENDMATH
        ->
          set_position text curseur pos;
          set_attr text curseur len yellow_attr;
          Normal_mode
      | STARTTABBING
      | ENDTABBING
      | KWDTABBING
        ->
          set_position text curseur pos;
          set_attr text curseur len gray_attr;
          mode
      | STARTARRAY
      | ENDARRAY
      | KWDARRAY
      | STARTENV
      | ENDENV
      | CONDITION    
      | ITEM
      | PARAMETER
      | COMMAND
      | FONT ->
          set_position text curseur pos;
          set_attr text curseur len red_attr;
          mode
      | LBRACE ->
          set_position text curseur pos;
          set_attr text curseur len red_attr;
          Arg_mode
      | RBRACE ->
          set_position text curseur pos;
          set_attr text curseur len red_attr;
          Normal_mode
      | _ -> 
          match mode with
            Math_mode -> 
              set_position text curseur pos;
              set_attr text curseur len yellow_attr;
              mode
          | Arg_mode ->
              set_position text curseur pos;
              set_attr text curseur len gray_attr;
              mode
          | _ -> mode
    in
    iter mode lexbuf
  in
  try
    iter Normal_mode lexbuf
  with
    _ ->
      buf.buf_modified <- buf.buf_modified + 1;
      remove_point text curseur;
      remove_point text end_point;
      remove_point text start_point


let c_c = (ControlMap,Char.code 'c')

let structures = define_option ["tex_mode"; "structures"] ""
    (list_option binding_option) []
  
let _ =
  if !!structures = [] then 
    structures =:=
      [
      [c_c;NormalMap, Char.code '0'], "\\documentclass[twocolumn^^]{article}\n%^^\\usepackage{isolatin1}\n%^^\\usepackage{francais}\n\\usepackage{url}\n\\usepackage[dvips]{epsfig}\n\n\\begin{document}\n\\title{^^}\n\\bibliographystyle{plain}\n\author{^^}\n\\maketitle\n\\abstract{^^}\n\\section{Introduction}\n^^\n\\bibliography{^^}\n\\end{document}\n";
      [c_c;NormalMap, Char.code '1'], "\\section{^^}\n^^";
      [c_c;NormalMap, Char.code '2'], "\\subsection{^^}\n^^";
      [c_c;NormalMap, Char.code '3'], "\\subsubsection{^^}\n^^";
      [c_c;ControlMap, Char.code '1'], "\\section*{^^}\n^^";
      [c_c;ControlMap, Char.code '2'], "\\subsection*{^^}\n^^";
      [c_c;ControlMap, Char.code '3'], "\\subsubsection*{^^}\n^^";
      [c_c;NormalMap, Char.code 'e'], "\\begin{^^}\n^^\n\\end{^^}\n^^";
      [c_c; NormalMap, Char.code 'i'], "\\begin{itemize}\n\\item ^^\n\\end{itemize}";    
      [c_c; NormalMap, Char.code 'v'], "\\begin{verbatim}\n ^^\n\\end{verbatim}";    
    ]
    
let tex_error_regexp = define_option ["tex_mode"; "error_regexp"] ""
    regexp_option (string_to_regex "^l.\([0-9]+\)")
let tex_error_regexp2 = define_option ["tex_mode"; "warning_regexp"] ""
    regexp_option (string_to_regex "^LaTeX Warning:.*line +\([0-9]+\)")
  
let backward_find_unclosed_paren text point =
  let rec iter n =
    if bmove_res text point 1 = 0 then raise Not_found;
    let d = get_char text point in
    if d = '\n' then iter_line n else
    if d = ')' then iter (n+1) else
    if d = '(' then
      (if n > 0 then iter (n-1))
    else iter n
  and iter_line n =
    let p = get_position text point in
    let bol = point_to_bol text point in
    set_position text point (p-bol);
    if sub text point 4 = "\\OT1" then
      (if bmove_res text point 1 = 0 then raise Not_found; iter_line n)
    else
      (set_position text point p; iter n)
  in
  iter_line 0 

let filename_after_point text point =
  let endp = dup_point text point in
  let rec iter () = 
    if fmove_res text endp 1 = 1 &&
      (let d = get_char text endp in
        d <> '\n' && d <> ')' && d <> ' ') then iter ()
  in
  iter ();
  let filename = Text.sub text point (distance text point endp) in
  remove_point text endp;
  filename

  
  
let tex_find_error text error_point = 
  let groups = 
    try
      Text.search_forward_groups text (snd !!tex_error_regexp)
        error_point 1 
    with
      Not_found ->
        Text.search_forward_groups text (snd !!tex_error_regexp2)
          error_point 1 
  in
  let point = dup_point text error_point in
  let filename = 
    try
      backward_find_unclosed_paren text point;
      fmove text point 1;
      filename_after_point text point
    with
      Not_found -> ""
  in
  remove_point text point;
  for i = 1 to 3 do
    bmove text error_point 1;
    let bol = point_to_bol text error_point in
    bmove text error_point bol;
  done;
  { err_msg = Text.get_position text error_point;
      err_filename = filename;
      err_line = int_of_string groups.(0) - 1;
      err_begin = 0;
      err_end = 0
      }

  
let syntax = define_option ["tex_mode"; "syntax"] 
    "Chars which should not are part of idents" 
    string_option "\'-"

let tex_hooks = define_option ["tex_mode"; "hooks"] "" 
  (list_option string_option)
  [ "accents_mode"; "fill_mode" ]
  
let install buf =
  tex_color_buffer buf; 
  (* les \ et ' font parties des mots *)
  let syntax = !!syntax in
  for i = 0 to String.length syntax - 1 do
    buf.buf_syntax_table.(Char.code syntax.[i]) <- true;
  done;
  let abbrevs = try get_local buf abbrev_table
    with _ -> 
        let abbrevs = Hashtbl.create 11 in
        set_local buf abbrev_table abbrevs;
        abbrevs
  in
  Utils.hash_add_assoc abbrevs !!abbreviations;
  install_structures buf !!structures; 
  List.iter (fun action ->
      try execute_buffer_action action buf with _ -> ()
  ) !!tex_hooks;
  ()
  
let mode = Ebuffer.new_major_mode"TeX" [install]

let comment_string = "%"
let _ = set_major_var mode line_comment "%"
  
let comment_region frame =
  let buf = frame.frm_buffer in
  let point = frame.frm_point in
  let text = buf.buf_text in
  let len = String.length comment_string in
  let mark = 
    match buf.buf_mark with None -> failwith "Mark is not set"
    | Some p ->  p in
  let start, stop = 
    if compare text point mark < 0 then point, mark else mark, point in
  let session = start_session text in
  while compare text start stop < 0 do
    let c = get_char text start in
    fmove text start 1;
    if c = '\n' then (insert text start comment_string; fmove text start len)
  done;
  commit_session text session
  
let uncomment_region frame =
  let buf = frame.frm_buffer in
  let point = frame.frm_point in
  let text = buf.buf_text in
  let mark = 
    match buf.buf_mark with None -> failwith "Mark is not set"
    | Some p ->  p in
  let len = String.length comment_string in
  let start, stop = 
    if compare text point mark < 0 then point, mark else mark, point in
  let session = start_session text in
  while compare text start stop < 0 do
    let c = get_char text start in
    fmove text start 1;
    if c = '\n' && sub text start len = comment_string then 
      delete text point len
  done;
  commit_session text session
  
let insert_return_in_tex frame =
  let buf = frame.frm_buffer in
  let point = frame.frm_point in
  let text = buf.buf_text in
  let bol = point_to_bol text point in
  bmove text point bol;
  let comment = (get_char text point = '%') && bol <> 0 in
  fmove text point bol;
  insert_return frame;
  if comment then insert_char frame '%'

  (* load in buffer \input{file} *)
let load_input frame buf point =
  let str = 
    let text = buf.buf_text in
    let curs = dup_point text point in
    let dirname = match buf.buf_filename with
        None -> ""
      | Some filename -> Filename.dirname filename
    in
    try
      let bol = point_to_bol text curs in
      bmove text curs bol;
      while sub text curs 7 <> "\input{" do
        let c = get_char text curs in
        if c = '\n' || fmove_res text curs 1 = 0 then raise Not_found;
      done;
      fmove text curs 7;
      let pos = get_position text curs in
      let rec iter count =
        let c = get_char text curs in
        if c = '}' then count else
        if c = '\n' || fmove_res text curs 1 = 0 then raise Not_found 
        else iter (count+1)
      in
      let len = iter 0 in
      set_position text curs pos;
      let s = sub text curs len in
      remove_point text curs;
      Filename.concat dirname (s^".tex")
    with
      e -> remove_point text curs; raise e
  in
  let _ = Frame.load_file frame.frm_window str in ()

let load_input_file frame = 
  Select.set_previous_frame frame;
  let buf= frame.frm_buffer in
  load_input frame buf frame.frm_point

let input_regexp = Str.regexp "\\input{"
let main_buffer = ref None
let set_main_file frame = main_buffer := Some frame.frm_buffer
let to_main_file frame =
  let buf_name = match !main_buffer with None -> raise Not_found |
      Some buf -> buf.buf_name in
  Select.set_previous_frame frame;
  Frame.change_buffer frame.frm_window buf_name
 
let load_next_input_file frame =
  let buf = match !main_buffer with
      None -> raise Not_found | Some buffer -> buffer in
  let text = buf.buf_text in
  let point = match buf.buf_mark with
      None -> add_point text | Some mark -> mark in
  buf.buf_mark <- Some point;
  let len = search_forward text input_regexp point in
  fmove text point len;
  load_input frame buf point

let load_prev_input_file frame =
  let buf = match !main_buffer with
      None -> raise Not_found | Some buffer -> buffer in
  let text = buf.buf_text in
  let point = match buf.buf_mark with
      None -> add_point text | Some mark -> mark in
  buf.buf_mark <- Some point;
  let len = search_backward text input_regexp point in
  load_input frame buf point

let browse frame =
  let buf = frame.frm_buffer in
  let filename = match buf.buf_filename with
      None -> failwith "Buffer not saved"
    | Some filename -> filename in
  Server.start frame;
  let _ = Sys.command (Printf.sprintf "efuns_texbrowser %s &" filename) in
  ()
  
  
let tex_mode frame = Ebuffer.set_major_mode frame.frm_buffer mode

let env_list = Options.define_option ["tex_mode";"env_list"] ""
    (list_option string_option) ["array"; "eqnarray";"figure"]
let env_hist = ref !!env_list
  
let begin_env frame = 
  let default = match !env_hist with  [] -> "" | h :: _ -> h in
  Select.select frame ("Insert env: ["^default^"] ") env_hist ""
    (fun _ -> !env_hist) (fun s -> s) (fun str ->
      let str = if str = "" then default else str in
      let buf = frame.frm_buffer in
      let text = buf.buf_text in
      let point = frame.frm_point in
      insert text point (Printf.sprintf "\\begin{%s}\n\\end{%s}" str str);
      Text.fmove text point (8 + String.length str);
      ()
  )
  
(* Could be improved to avoid nested envs *)
let begin_env_regexp = Str.regexp "\\begin{\([^}]*\)}"
let end_env frame = 
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let find_point = dup_point text point in
  try
    let names = Text.search_backward_groups text begin_env_regexp find_point 1
    in
    remove_point text find_point;
    insert text point (Printf.sprintf "\\end{%s}\n" names.(0));
    fmove text point (String.length names.(0) + 7)    
  with e -> remove_point text find_point; raise e
  
  
let _ = 
  define_action "tex_mode" tex_mode;
  define_action "tex_mode.browse" browse;
  define_action "tex_mode.color_buffer" (fun frame -> 
      tex_color_buffer frame.frm_buffer);
  define_action "tex_mode.compile" (compile tex_find_error);
  define_action "tex_mode.comment_region" comment_region;
  define_action "tex_mode.uncomment_region" uncomment_region;
  define_action "tex_mode.char_expand_abbrev" (fun frame ->
      expand_sabbrev frame; self_insert_command frame);
  define_action "tex_mode.insert_return" insert_return_in_tex;
  define_action "tex_mode.load_input_file" load_input_file;
  define_action "tex_mode.load_next_input_file" load_next_input_file;
  define_action "tex_mode.load_prev_input_file" load_prev_input_file;
  define_action "tex_mode.set_main_file" set_main_file;
  define_action "tex_mode.to_main_file" to_main_file;
  define_action "tex_mode.find_matching_paren" (fun frame ->
      self_insert_command frame;
      highlight_paren frame);
  define_action "tex_mode.end_env" end_env;
  define_action "tex_mode.begin_env" begin_env;
  ()  
    
let local_map = define_option ["tex_mode"; "local_map"] ""
    (list_option binding_option) []

let interactives_map = define_option ["tex_mode"; "interactives_map"] ""
    (list_option string2_option) 
  []

let _ =
  if !!local_map = [] then
    local_map =:= [
      [c_c;ControlMap, Char.code 'l'], "tex_mode.color_buffer" ;
      [c_c;ControlMap, Char.code 'c'], "tex_mode.compile" ;
      [c_c;NormalMap, Char.code '%'], "tex_mode.comment_region";
      [c_c;ControlMap, Char.code 'u'; NormalMap,Char.code '%'],
      "uncomment_region";
      [NormalMap, Char.code '{'], "tex_mode.char_expand_abbrev";
      [NormalMap, Char.code '*'], "tex_mode.char_expand_abbrev";
      [NormalMap, Char.code ' '], "tex_mode.char_expand_abbrev";
      [MetaMap, Char.code 'q'], "fill_paragraph";
      [NormalMap, XK.xk_Return], "tex_mode.insert_return";  
      
      [c_c; NormalMap,XK.xk_bracketleft], "tex_mode.begin_env";
      [c_c; NormalMap,XK.xk_bracketright], "tex_mode.end_env";
      [c_c; NormalMap,XK.xk_Return], "tex_mode.load_input_file";
      [c_c; NormalMap,XK.xk_Right], "load_next_input_file";
      [c_c; NormalMap,XK.xk_Left], "load_prev_input_file";
      [c_c; NormalMap,XK.xk_Down], "set_main_file";
      [c_c; NormalMap,XK.xk_Up], "to_main_file";
      
      [NormalMap, Char.code '}'], "tex_mode.find_matching_paren";
      [NormalMap, Char.code ']'], "tex_mode.find_matching_paren";
      [NormalMap, Char.code ')'], "tex_mode.find_matching_paren";
    ];
  if !!interactives_map = [] then 
    interactives_map =:= [
      "tex_browser", "tex_mode.browse";
      "color_buffer", "tex_mode.color_buffer";
    ]

let _ =
  let map = mode.maj_map in
  (*  Keymap.add_prefix map [c_c]; *)
  List.iter (fun (keys, action) ->
      try
        let f = execute_action action in
        Keymap.add_binding map keys f;
        add_interactive map action f;
      with e ->
          Log.printf "Error for action %s" action;
          Log.exn "%s\n" e;
  
  ) !!local_map;
  List.iter (fun (name, action) ->
      try
        add_interactive map name (execute_action action)
      with e ->
          Log.printf "Error for action %s" action;
          Log.exn "%s\n" e;          
  ) !!interactives_map;
  ()
  
let mode_regexp = define_option ["tex_mode"; "mode_regexp"] ""
    (list_option string_option) [ ".*\.tex"; ".*\.cls"; ".*\.sty" ]


  
let _ =  
  Efuns.add_start_hook (fun location ->
      let alist = get_global location Ebuffer.modes_alist in
      set_global location Ebuffer.modes_alist 
        ((List.map (fun s -> s,mode) !!mode_regexp) @ alist);
      
      add_option_parameter location keyword_color;
      add_option_parameter  location string_color;
      add_option_parameter location comment_color;
      add_option_parameter location  upper_color;
  )
  
} 
