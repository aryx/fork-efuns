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

(* A FAIRE:
On doit faire converger ce text vers un text directement affichable par
WX_text. Pour cela:

Modifier WX_text pour qu'il se satisfasse de l'interface de Text.
En particulier, il doit n'utiliser que les fonctions suivantes:

Text.representation text line --> string (buffer or copy) * int (position)
Text.items line -> item array
  
Il doit aussi utiliser la structure un peu particuliere de l'arbre utiliser
ici.
  
*)

open Options
open Utils
open WX_types
open WX_text
  
type attribute = int
type delta = int
type position = int

type direct = int
type session = int

and line = {
    mutable position : direct;
    mutable representation : repr list;
    mutable modified : int; (* first modified position *)
    mutable repr_len : int;
    mutable repr_string : string;
    mutable line_hlt : int;
    (* hightlighting on the line:
    0 => no hightlighting
    n => chars are hightlighted from the beginning of the line until pos n
    *)
    mutable items : item array;
  } 

and point = {
    mutable point : int;
    mutable point_y : int;
  }

and repr = 
  { 
    repr_line_pos : int;   (* pos of repr in Text.t string *)
    repr_line_len : int;   (* len of repr in Text.t string *)
    
    mutable repr_attr : int;    (* common attribute *)
    repr_charsize : int; (* common size *)
    
    repr_size : int;
    repr_pos : int;  (* pos of repr in representation string *)
  } 

type text = {
    mutable text_string : string;
    mutable text_attrs : int array;
    mutable text_size : int;
    
    mutable text_points : point list;
    
    mutable text_gpoint : int;
    mutable text_gline : int;
    mutable text_gsize : int;
    
    mutable text_newlines : line array;
    mutable text_nlines : int;
    
    mutable text_modified : int;
    mutable text_clean : bool;
    
    mutable text_readonly : bool;
    mutable text_history : action list;
  } 
  
and  action =
  Insertion of int * int * int
| Deletion of int * string * int
| Session of action list

module Text = struct
    type t = text
    type text = t
    type l = line
    type line = l
      
    let representation tree line = line.repr_string, line.position
    let items tree line = line.items
  end
      
module TextTree = WX_text.Make(Text)
open TextTree
  
type t = tree tree_desc  

external id: t -> tree tree_desc = "%identity"
  
(*
let print msg text =
  let s = text.text_string in
  let gpoint = text.text_gpoint in
  let gsize  = text.text_gsize in
  let len = text.text_size in
  let gap_end = gpoint + gsize in
    Printf.printf "%s: <<%s[gap:%d]%s>>" msg (String.sub s 0 gpoint) gsize 
      (String.sub s gap_end (len - gap_end));
    print_newline ()
 
let print_newlines text =
  print_string "Newlines :";
  for i = 0 to text.text_nlines - 1 do
    Printf.printf " %d" text.text_newlines.(i).position;
  done;
  print_newline ()
*)

let version tree = 
    let text = tree.tree_text in    
  text.text_modified
  
let nbre_lines tree = 
  let text = tree.tree_text in    
  text.text_nlines - 2
  
let size tree = 
  let text = tree.tree_text in    
  text.text_size - text.text_gsize
  
let point_col tree point = 
  let text = tree.tree_text in    
  let gpoint = text.text_gpoint in
  let bol = text.text_newlines.(point.point_y).position in
  let point = point.point in
  if point > gpoint && bol <= gpoint then
    point - bol - text.text_gsize
  else
    point - bol

let make_attr fg bg font highlighted =
  let attr = fg + (bg lsl 8) + (font lsl 16) + 
      (if highlighted then 1 lsl 24 else 0) 
  in
  attr

let direct_attr =  make_attr 0 1 0 false
let inverse_attr =  make_attr 1 0 0 false

let move_gpoint_to text point =
  let gpoint = text.text_gpoint in
  let gsize = text.text_gsize in
  let gline = text.text_gline in
  text.text_clean <- false;
  if point <> gpoint then 
    let gap_end = gpoint + gsize in
    if point < gpoint then
      let delta = gpoint - point in
      let (delta_line,_) = count_char_sub text.text_string
          point delta '\n' in
      String.blit text.text_string point 
        text.text_string (point + gsize) delta;
      Array.blit text.text_attrs point 
        text.text_attrs (point + gsize) delta;
      for i = gline - delta_line + 1 to gline do
        text.text_newlines.(i).position
          <- text.text_newlines.(i).position + gsize 
      done;
      List.iter (fun p -> 
          if p.point > point && p.point <= gpoint then
            p.point <- p.point + gsize
      ) text.text_points;
      text.text_gpoint <- point;
      text.text_gline <- gline - delta_line;
    else
    let delta = point - gap_end in
    let (delta_line,_) = 
      count_char_sub text.text_string gap_end delta '\n' in
    String.blit text.text_string gap_end text.text_string gpoint delta;
    Array.blit text.text_attrs gap_end text.text_attrs gpoint delta;
    for i = gline + 1 to gline + delta_line do
      text.text_newlines.(i).position
        <- text.text_newlines.(i).position - gsize
    done;
    List.iter (fun p -> 
        if p.point >= gap_end && p.point <= point then
          p.point <- p.point - gsize
    ) text.text_points;
    text.text_gpoint <- point - gsize;
    text.text_gline <- gline + delta_line

let cancel_repr text point n =
  let line = text.text_newlines.(n) in
  let pos =  point - line.position in
  line.modified <- 
    (if line.modified < 0 then pos
    else min line.modified pos)

let add_amount = define_option ["add_amount"] "Size of the gap in the buffer"
  int_option 200
let extend_gap text amount =
  let add_size = max !!add_amount 
      ((amount / !!add_amount) * !!add_amount + !!add_amount) in
  let old_size = text.text_size in
(* use String.create here *)
  let new_text = String.create (old_size + add_size) in
  let new_attrs = Array.create (old_size + add_size) direct_attr in
  let gpoint = text.text_gpoint in
  let gsize = text.text_gsize in
  let gap_end = gpoint + gsize in
  String.blit text.text_string 0 new_text 0 gpoint; 
  Array.blit text.text_attrs 0 new_attrs 0 gpoint; 
  String.blit text.text_string gap_end
    new_text (gap_end + add_size) 
  (old_size - gap_end);
  Array.blit text.text_attrs gap_end
    new_attrs (gap_end + add_size) 
  (old_size - gap_end);
  for i = text.text_gline + 1 to text.text_nlines - 1 do
    text.text_newlines.(i).position <- 
      text.text_newlines.(i).position + add_size
  done;
  List.iter (fun p -> 
      if p.point > gpoint then
        p.point <- p.point + add_size
  ) text.text_points;
  text.text_gsize <- gsize + add_size;
  text.text_size <- old_size + add_size;
  text.text_string <- new_text;
  text.text_attrs <- new_attrs

exception ReadOnlyBuffer

let tree_insert tree t gline nbr = ()
(*
  let rec iter tree lines =
    match tree with
      Parts text ->
        if lines = text.tree_nlines || lines = -1 then
          (* Insert in the last part *)
          iter text.tree_parts.(Array.length text.tree_parts - 1) (-1)
        else
        let rec iter2 lines i =
          let tlines = match text.tree_parts.(i) with
              Parts t -> t.tree_nlines
            | Lines t -> t.tree_nlines 
          in
          if tlines > lines then
            iter text.tree_parts.(i) lines
          else
            iter2 (lines - tlines) (i+1)
        in
        iter2 lines 0;
        text.tree_modified <- true;
        text.tree_nlines <- text.tree_nlines + nbr
    | Lines text ->
        let newtext = Array.create text.tree_nlines text.tree_parts.(0) in
        Array.blit text.tree_parts 0 newtext 0 lines;
        Array.blit t.text_newlines gline newtext lines nbr;
        Array.blit text.tree_parts lines newtext (lines+nbr) (
          text.tree_nlines - lines);
        text.tree_modified <- true;        
        text.tree_nlines <- text.tree_nlines + nbr
  in
  iter (Parts tree) gline
*)

let low_insert tree point str =
  let text = tree.tree_text in
  if text.text_readonly then failwith "Buffer is read-only";
  move_gpoint_to text point;
  let gpoint = text.text_gpoint in
  let gsize = text.text_gsize in
  let gline = text.text_gline in
  let gchars = gpoint - text.text_newlines.(gline).position in
  cancel_repr text gpoint gline;
  let strlen = String.length str in
  if strlen > gsize then extend_gap text strlen;
  let gsize = text.text_gsize in
  String.blit str 0 text.text_string gpoint strlen;
  Array.fill text.text_attrs gpoint strlen direct_attr;
  let (nbr_newlines,nbr_chars) = count_char str '\n' in
  if nbr_newlines > 0 then
    begin
      if (Array.length text.text_newlines - text.text_nlines)
        < nbr_newlines then
        begin
          let old_size = text.text_nlines in
          let new_cache = Array.create (old_size + (max 20 nbr_newlines)) 
            { position = -1;
              representation = [];
              modified = -1;
              repr_len = 0;
              repr_string = "";
              line_hlt = 0;
              items = [||];
            } 
          in
          Array.blit text.text_newlines 0 new_cache 0 old_size;
          text.text_newlines <- new_cache;
        end;
      Array.blit text.text_newlines (gline+1) 
      text.text_newlines (gline+1+ nbr_newlines) 
      (text.text_nlines - gline -1);
      text.text_nlines <- text.text_nlines + nbr_newlines;
      let rec iter n pos =
        let new_pos = String.index_from text.text_string pos '\n' in
        text.text_newlines.(gline+n) <- { position = (new_pos + 1);
          representation = [];
          modified = 0;
          repr_len = 0;
          repr_string = "";
          line_hlt = 0;
          items = [||];
        };
        if n < nbr_newlines then
          iter (n+1) (new_pos + 1)
      in
      iter 1 gpoint;
      tree_insert tree text text.text_gline nbr_newlines;
    end;
  let gline = text.text_gline in
  List.iter (fun p ->
      if p.point > gpoint then
        begin
          if p.point_y = gline then 
            (* p.point_x <- (p.point_x - (if nbr_newlines > 0 then gchars else 0)) + nbr_chars; *)
            p.point_y <- p.point_y + nbr_newlines;
        end
  ) text.text_points;
  text.text_gpoint <- gpoint + strlen;
  text.text_gsize <- gsize - strlen;
  text.text_gline <- gline + nbr_newlines;
  (gpoint,strlen,text.text_modified) 

let low_delete tree point len =
  let text = tree.tree_text in      
  if text.text_readonly then failwith "Buffer is read-only";
  move_gpoint_to text point;
  let gsize = text.text_gsize in
  let size = text.text_size in
  let gpoint = text.text_gpoint in
  let gline = text.text_gline in
  cancel_repr text gpoint gline;
  let gchars = gpoint - text.text_newlines.(gline).position in
  let gap_end = gpoint + gsize in
  let len = min (size - gap_end) len in
  let str = String.sub text.text_string gap_end len
  in
  let (nbr_newlines, nbr_chars) = count_char str '\n' in
  if nbr_newlines > 0 then
    begin
      Array.blit text.text_newlines (gline + nbr_newlines + 1)
      text.text_newlines (gline + 1) 
      (text.text_nlines - gline - nbr_newlines - 1);
      text.text_nlines <- text.text_nlines - nbr_newlines;
    end;
  text.text_gsize <- gsize + len;
  List.iter (fun p -> 
      if p.point > gap_end + len then
        begin
          (*if p.point_y = gline + nbr_newlines then
            p.point_x <- (p.point_x - nbr_chars) + 
              (if nbr_newlines > 0 then gchars else 0);*)
          p.point_y <- p.point_y - nbr_newlines;
        end 
      else
      if p.point > gpoint then
        ( p.point <- gpoint;
          (* p.point_x <- gchars; *)
          p.point_y <- gline);
  ) text.text_points;
  (gpoint,str,text.text_modified) 

let undo tree =
  let text = tree.tree_text in  
  let rec undo action =
    let gpoint = text.text_gpoint in
    let gsize = text.text_gsize in
    match action with
      Insertion(point_pos, len, modified) ->
        let point = if gpoint < point_pos then point_pos + gsize else point_pos in
        let (pos,str,modif) = low_delete tree point len in
        text.text_modified <- modified;
        Deletion(pos,str,modif), point_pos, 0
    | Deletion (point_pos, str, modified) ->
        let point = if gpoint < point_pos then point_pos + gsize else point_pos in
        let (pos,len,modif) = low_insert tree point str in
        text.text_modified <- modified;
        Insertion(pos,len,modif), point_pos, String.length str
    | Session actions ->
        let last_point = ref 0 in
        let last_len = ref 0 in
        let rev_actions =
          List.fold_left (fun undos action -> 
              let (rev_action,point,len) = undo action in
              last_point := point;
              last_len := len;
              rev_action :: undos
          ) [] actions
        in
        Session rev_actions, !last_point, !last_len
  in
  match text.text_history with
    [] -> raise Not_found
  | action :: tail -> 
      let rev_action = undo action in
      text.text_history <- tail;
      rev_action

let insert_at_end tree str =
  let text = tree.tree_text in
  let _ = low_insert tree text.text_size str in
  text.text_history <- [];
  text.text_modified <- text.text_modified + 1

let insert_res tree point str =
  let text = tree.tree_text in
  let (pos,len,modif) = low_insert tree point.point str in
  text.text_history <- Insertion(pos,len,modif) :: text.text_history;
  text.text_modified <- text.text_modified + 1;
  pos, len

let insert text point str = let _ = insert_res text point str in ()
  
let delete_res tree point len =
  let text = tree.tree_text in  
  let (pos,str,modif) = low_delete tree point.point len in
  text.text_history <- Deletion(pos,str,modif) :: text.text_history;
  text.text_modified <- text.text_modified + 1;
  pos, str

let delete text point len = let _ = delete_res text point len in ()
  
let compute_newlines string =
  let (nbr_newlines,_) = count_char string '\n' in
  let newlines = Array.create (nbr_newlines + 2) 
    { position = 0; representation = []; modified = 0; repr_len = 0; repr_string = ""; line_hlt = 0; items = [||]; } in
  let curs = ref 0 in
  for i = 1 to nbr_newlines do
    let pos = String.index_from string !curs '\n' in
    newlines.(i) <- { position = pos+1; representation = []; 
      modified = 0; repr_len = 0; repr_string = "";
      line_hlt = 0; items = [||];
    };
    curs := pos + 1;
  done;
  newlines.(nbr_newlines+1) <- { position = String.length string + 1; 
    representation = []; modified = 0; 
    repr_len = 0; repr_string = "";
    line_hlt = 0; items = [||];
  };
  newlines

let create str =
  let newlines = compute_newlines str in
  let nlines = Array.length newlines in
  let attrs = (Array.create (String.length str) direct_attr) in
  let rec text =
    {
      text_string = str;
      text_attrs = attrs;
      text_size = String.length str;
      
      text_points = [];
      
      text_gpoint = 0;
      text_gline = 0;
      text_gsize = 0;
      
      text_newlines = newlines;
      text_nlines = nlines;
      
      text_modified = 0;
      text_clean = false;
      text_history = [];
      text_readonly = false;
    }
  in
  make_text text (Array.copy newlines)
  
let find_xy text point line pos =
  let text = text.tree_text in    
  let gpoint = text.text_gpoint in
  let gline = text.text_gline in
  let gap_end = gpoint + text.text_gsize in
  let y,x =
    if pos >= gap_end then
(* go forward *)
      let rec iter line =
        if line >= text.text_nlines then
          text.text_nlines - 1
        else
        if text.text_newlines.(line).position > pos then line - 1
        else
          iter (line + 1)
      in
      let line = 
        if point > gap_end && pos > point then
          iter (line+1) 
        else
          iter (gline+1) 
      in
      if line = gline then
        let gchars = gpoint - text.text_newlines.(gline).position in
        line, gchars + pos - gap_end
      else
        line, pos - text.text_newlines.(line).position
    else
(* go backward *)
    let rec iter line =
      if line > 0 then
        if text.text_newlines.(line).position > pos then 
          iter (line - 1)
        else
          line
      else
        0
    in
    let line = 
      if point < gpoint && pos <= point then
        iter line 
      else
        iter gline in
    line, pos - text.text_newlines.(line).position
  in
  x,y

let add_point tree =
  let text = tree.tree_text in      
  let point = {
      point = 0;
      point_y = 0;
    } in    
  text.text_points <- point :: text.text_points;
  point

let dup_point tree point =
  let text = tree.tree_text in      
  let p = {
      point = point.point;
      point_y = point.point_y
    } in
  text.text_points <- p :: text.text_points;
  p

let goto_point text p1 p2 =
  p1.point <- p2.point;
  p1.point_y <- p2.point_y

let move_point_to tree point p =
  let text = tree.tree_text in    
  let gpoint = text.text_gpoint in
  let gline = text.text_gline in
  let x,y = find_xy tree gpoint gline p in
  point.point <- p;
  point.point_y <- y

let remove_point tree p =
  let text = tree.tree_text in      
  text.text_points <- 
    (List.fold_left (fun points point ->
        if point == p then points else point :: points) 
    [] text.text_points)

let read inc =
  create (read_string inc)

let save tree outc =
  let text = tree.tree_text in    
  let str = text.text_string in
  let gpoint = text.text_gpoint in
  let gsize = text.text_gsize in
  output outc str 0 gpoint;
  output outc str (gpoint + gsize) 
  (text.text_size - gpoint - gsize)

let unset_attr text =
  let text = text.tree_text in  
  Array.fill text.text_attrs 0 (Array.length text.text_attrs) direct_attr;
  Array.iter (fun line -> line.modified <- 0) text.text_newlines

let set_attr tree point len attr = (* should not exceed one line *)
  let text = tree.tree_text in  
  if len > 0 then
    let point = point.point in
    let gpoint = text.text_gpoint in
    let gline = text.text_gline in
    let size = text.text_size in
    let gap_end = gpoint + text.text_gsize in
    let x,y = find_xy tree gpoint gline point in
    cancel_repr text point y;
    let before, after, after_pos =
      if point > gap_end then
        0, (min (size - point) len), point
      else
      if point + len <= gpoint then
        0, len, point
      else
      let before = gpoint - point in
      let after = min (len - before) (size - gap_end) in
      before, after, gap_end
    in
    let attrs = text.text_attrs in
    if before > 0 then Array.fill attrs point before attr;
    Array.fill attrs after_pos after attr

let low_distance text p1 p2 =
  if p1 >= p2 then 0 else
  if p1 <= text.text_gpoint then
    if p2 <= text.text_gpoint then
      p2 - p1
    else
      p2 - p1 - text.text_gsize
  else
  if p2 <= text.text_gpoint then
    p2 - p1 + text.text_gsize
  else
    p2 - p1

let distance tree p1 p2 =
  let text = tree.tree_text in    
  low_distance text p1.point p2.point

let compare text p1 p2 = compare p1.point p2.point
  
let add text point delta =
  let gpoint = text.text_gpoint in
  let gap_end = gpoint + text.text_gsize in
  if point <= gpoint && point + delta > gpoint then
    point + delta + text.text_gsize
  else
  if point >= gap_end && point+delta < gap_end then
    point + delta - text.text_gsize
  else
    point + delta

let get_char tree point =
  let text = tree.tree_text in    
  let point = point.point in
  let gpoint = text.text_gpoint in
  let gsize = text.text_gsize in
  let size = text.text_size in
  let string = text.text_string in
  let point = 
    if point = gpoint then point + gsize else point
  in
  if point < size then
    string.[point]
  else
    '\000'

let get_attr tree point =
  let text = tree.tree_text in    
  let point = point.point in
  let gpoint = text.text_gpoint in
  let gsize = text.text_gsize in
  let size = text.text_size in
  let attrs = text.text_attrs in
  let point = 
    if point = gpoint then point + gsize else point
  in
  if point < size then
    attrs.(point)
  else
    direct_attr


let set_char_attr tree point attr =
  let text = tree.tree_text in    
  let line = point.point_y in
  let point = point.point in
  let gpoint = text.text_gpoint in
  let gsize = text.text_gsize in
  let size = text.text_size in
  let attrs = text.text_attrs in
  let point = 
    if point = gpoint then point + gsize else point
  in
  if point < size then
    (cancel_repr text point line;
      attrs.(point) <- attr)

let fmove_res tree p delta =
  let text = tree.tree_text in    
  if delta = 0 then 0 else
  let gpoint = text.text_gpoint in
  let gsize = text.text_gsize in
  let size = text.text_size in
  let gap_end = gpoint + text.text_gsize in
  let gline = text.text_gline in
  let point = p.point in
  let old_point = point in
  let string = text.text_string in
  let lines = text.text_newlines in
  let rec iter y point end_point =
    if end_point > point then
      let end_line = lines.(y+1).position in
      if end_point >= end_line then
        iter (y+1) end_line end_point
      else
        (y, end_point)
    else
      (y,point)
  in
  let (y,point) = 
    if point + delta <= gpoint then
      iter p.point_y point (point+delta)
    else
    if point >= gap_end then
      let delta = min delta (size - point) in
      iter p.point_y point (point + delta)
    else
    let delta = min (delta - (gpoint - point)) (size - gap_end) in
    iter gline gap_end
      (gap_end + delta) 
  in
  p.point <- point;
  p.point_y <- y;
  low_distance text old_point point



let bmove_res tree p delta =
  let text = tree.tree_text in    
  if delta = 0 then 0 else
  let gpoint = text.text_gpoint in
  let gsize = text.text_gsize in
  let size = text.text_size in
  let gap_end = gpoint + text.text_gsize in
  let gline = text.text_gline in
  let point = p.point in
  let old_point = point in
  let string = text.text_string in
  let lines = text.text_newlines in
  let rec iter y end_point =
    let start_line = lines.(y).position in
    if end_point >= start_line then
      (y, end_point)
    else
      iter (y-1) end_point
  in
  let (y,point) = 
    if point - delta >= gap_end then
      if point - delta < lines.(gline+1).position then
        (
          gline, point - delta)
      else
        iter p.point_y (point-delta)
    else
    if point <= gpoint then
      let delta = min delta point in
      iter p.point_y (point - delta)
    else
    let delta = min (delta - (point - gap_end)) gpoint in
    iter gline (gpoint - delta) 
  in
  p.point <- point;
  p.point_y <- y;
  low_distance text point old_point

let bmove text p delta = 
  let _ = bmove_res text p delta in ()

let fmove text p delta = 
  let _ = fmove_res text p delta in ()
  
let to_string tree =
  let text = tree.tree_text in    
  let len = text.text_size - text.text_gsize in
  if len = 0 then "" else
  let str = String.create len in
  let gpoint = text.text_gpoint in
  let gap_end = gpoint + text.text_gsize in
  String.blit text.text_string 0 str 0 gpoint;
  String.blit text.text_string gap_end str gpoint (len- gpoint);
  str


let clean_text text =
  if not text.text_clean then
    let size = text.text_size in
    let gsize = text.text_gsize in
    let string = text.text_string in
    move_gpoint_to text size;
    String.fill string (size - gsize) gsize '\000';
    text.text_clean <- true


let blit str tree point len =
  let text = tree.tree_text in      
  let len = min len (low_distance text point.point text.text_size) in
  let gpoint = text.text_gpoint in
  let gap_end = gpoint + text.text_gsize in
  if point.point+len >= gpoint && point.point < gap_end then clean_text text;
  (try
    String.blit text.text_string point.point str 0 len
    with 
      e -> raise e);
  len
  
let get_position tree point = 
  let text = tree.tree_text in    
  if point.point > text.text_gpoint then
    point.point - text.text_gsize
  else
    point.point

let set_position tree point pos =
  let text = tree.tree_text in    
  move_point_to tree point
    (if pos > text.text_gpoint then pos + text.text_gsize
    else
      pos)
    
let sub text point len =
  let str = String.create len in
  let _ = blit str text point len in
  str
    
let search_forward tree regexp point =
  let text = tree.tree_text in    
  let gsize = text.text_gsize in
  let gap_end = text.text_gpoint + gsize in
  if point.point = text.text_gpoint then point.point <- gap_end;
  if point.point < gap_end then clean_text text;
  let gap_end = text.text_gpoint + gsize in  
  let string = text.text_string in
  let pos = Str.search_forward regexp string point.point in
  let pos = if pos >= gap_end then pos - gsize else pos in
  set_position tree point pos;
  Str.match_end () - Str.match_beginning ()

let replace_matched tree repl =
  let text = tree.tree_text in    
  Str2.replace_matched repl text.text_string
  
let search_forward_matched tree regexp point =
  let text = tree.tree_text in      
  let gsize = text.text_gsize in
  let gap_end = text.text_gpoint + gsize in
  if point.point = text.text_gpoint then point.point <- gap_end;
  if point.point < gap_end then clean_text text;
  let gap_end = text.text_gpoint + gsize in  
  let string = text.text_string in
  let pos = Str.search_forward regexp string point.point in
  let pos = if pos >= text.text_gpoint + gsize then pos - gsize else pos in
  set_position tree point pos;
  Str.matched_string string

let search_forward_groups tree regexp point groups =
  let text = tree.tree_text in      
  let gsize = text.text_gsize in
  let gap_end = text.text_gpoint + gsize in
  if point.point = text.text_gpoint then point.point <- gap_end;
  if point.point < gap_end then clean_text text;
  let gap_end = text.text_gpoint + gsize in  
  let string = text.text_string in
  let pos = Str.search_forward regexp string point.point in
  let pos = if pos >= gap_end then pos - gsize else pos in
  let array = Array.init groups (fun i -> Str.matched_group (i+1) string) in
  set_position tree point pos;
  array

let search_backward tree regexp point =
  let text = tree.tree_text in    
  if point.point > text.text_gpoint then clean_text text;
  let string = text.text_string in
  let start_pos =     
    if point.point > 0 then point.point - 1 
    else raise Not_found  in
  let pos =  Str.search_backward regexp string start_pos in
  set_position tree point pos;
  Str.match_end () - Str.match_beginning ()

let search_backward_groups tree regexp point groups =  
  let text = tree.tree_text in    
  if point.point > text.text_gpoint then clean_text text;
  let string = text.text_string in
  let start_pos =     
    if point.point > 0 then point.point - 1 
    else raise Not_found  in
  let pos =  Str.search_backward regexp string start_pos in
  let array = Array.init groups (fun i -> Str.matched_group (i+1) string) in
  set_position tree point pos;
  array


let repr_string = ref ""
let repr_size = ref 0

let (dummy_line : line) = 
  {
    position = max_int;
    representation = [];
    modified = 0;
    repr_len = 0;
    repr_string = "";
    line_hlt = 0;
    items = [||];
  } 

  
let tabreprs = [|
    "         ";
    "        ";
    "       ";
    "      ";
    "     ";
    "    ";
    "   ";
    "  ";
    " ";
    ""
  |]
      
(* On devrait reprendre la representation la ou elle est ... *)
let compute_representation tree charreprs n =
  let text = tree.tree_text in      
  if n >= text.text_nlines - 1 then 
    begin
      dummy_line.position <- text.text_size;
      dummy_line
    end
  else
  let line = text.text_newlines.(n) in
  if line.modified >= 0 then
    begin
      let start_pos = line.position in
      let end_pos = text.text_newlines.(n+1).position - 1
      in
      let len = low_distance text end_pos start_pos in
      let rec iter repr_list =
        match repr_list with
          repr :: tail ->
            let next_pos = repr.repr_line_pos + repr.repr_line_len in
            if next_pos < line.modified then
              repr_list, next_pos, repr.repr_pos + repr.repr_size
            else
              iter tail
        | [] ->
            [], 0, 0
      in
      let (repr_tail, next_pos, repr_pos) = iter line.representation in
      let repr_tail = ref repr_tail in
      let line_curs = ref (line.position + next_pos) in
      let line_start = ref next_pos in
      let repr_curs = ref repr_pos in
      let repr_start = ref repr_pos in
      let gpoint = text.text_gpoint in
      let gsize = text.text_gsize in
      let char_repr = ref "" in
      let char_size = ref 0 in
      repr_string := line.repr_string;
      repr_size := String.length line.repr_string;
      if !line_curs >= gpoint && 
        !line_curs < gpoint + gsize then 
        line_curs := !line_curs + gsize;
      while !line_curs < end_pos do
        let charattr = text.text_attrs.(!line_curs) in
        let char = Char.code text.text_string.[!line_curs] in      
        let charrepr =
          let char = Char.code text.text_string.[!line_curs] in      
          if char = 9 then tabreprs.(!repr_curs mod 9)
          else charreprs.(char) in
        let charsize = String.length charrepr in
        let line_len = ref 0 in
        (* for J.G. Malecki: tabs have a different representation depending
  on their position in the text (as in xterms) *)
        
        while !line_curs < end_pos && 
          (
            let char = Char.code text.text_string.[!line_curs] in
            char_repr := 
            (let char = Char.code text.text_string.[!line_curs] in      
              if char = 9 then tabreprs.(!repr_curs mod 9)
              else charreprs.(char));
            char_size := String.length !char_repr;
            !char_size == charsize && 
            charattr == text.text_attrs.(!line_curs)) do
          if !repr_curs + charsize >= !repr_size then
            begin
(* find a better heuristic to realloc the line string *)
              let new_len = !repr_size + 
                  (low_distance text end_pos !line_curs) + charsize * 2 
              in
              let new_repr = String.create new_len in
              String.blit !repr_string 0 new_repr 0 !repr_curs;
              repr_string := new_repr;
              repr_size := new_len;
            end;
          String.blit !char_repr 0 !repr_string !repr_curs charsize;
          repr_curs := !repr_curs + charsize;
          line_curs := !line_curs + 1;
          line_len := !line_len +1;
          if !line_curs = gpoint then line_curs := gpoint + gsize;
        done;
        let repr = {
            repr_line_pos = !line_start;
            repr_line_len = !line_len;
            
            repr_attr = charattr;
            repr_charsize = charsize;
            
            repr_size = !line_len * charsize;
            repr_pos = !repr_start;
          } in
        repr_start := !repr_curs;
        line_start := !line_start + !line_len;
        repr_tail := repr :: !repr_tail;
      done;
      line.representation <- !repr_tail;
      line.modified <- -1;
      line.repr_len <- !repr_curs;
      line.repr_string <- !repr_string;

      (* once we have computed the simple representation, we can add more
      complicated things, such as highlighting ... *)
      if line.line_hlt <> 0 then
        if line.line_hlt > 0 then
          (* the line is hightlighted from the beginning to pos the
            line.line_hlt char *)
          begin
            let first = line.line_hlt - 1 in
            let rec iter list tail =
              match list with
                [] -> List.rev tail 
              | repr :: list_r ->
                  if repr.repr_line_pos > first then
                    iter list_r (repr :: tail)
                  else
                  let len = first - repr.repr_line_pos + 1 in
                  (List.rev tail) @
                    (let before, after = 
                      if len = repr.repr_line_len then
                        [], list
                      else
                        [ 
                          { 
                            repr_attr = repr.repr_attr;
                            repr_charsize = repr.repr_charsize;
                            repr_line_pos = repr.repr_line_pos+len;
                            repr_line_len = repr.repr_line_len - len;
                            repr_size = repr.repr_charsize * (repr.repr_line_len - len);
                            repr_pos = repr.repr_pos + (len * repr.repr_charsize)
                          }
                        ], (
                          { 
                            repr_attr = repr.repr_attr;
                            repr_charsize = repr.repr_charsize;
                            repr_line_pos = repr.repr_line_pos;
                            repr_pos = repr.repr_pos;
                            repr_line_len = len;
                            repr_size = repr.repr_charsize * len;
                          }
                            :: list_r)
                    in
                    List.iter 
                      (fun repr ->
                        repr.repr_attr <- repr.repr_attr lor (1 lsl 24))
                    after;
                    before @ after)
            in
            line.representation <- iter line.representation []
          end
        else
        (* the line is hightlighted from then end to pos line.line_hlt *)
          begin
            let first = line.line_hlt - 1 in
            let rec iter list tail =
              match list with
                [] -> List.rev tail 
              | repr :: list_r ->
                  if repr.repr_line_pos > first then
                    iter list_r (repr :: tail)
                  else
                  let len = first - repr.repr_line_pos + 1 in
                  (List.rev tail) @
                    (let before, after = 
                      if len = repr.repr_line_len then
                        [], list
                      else
                        [ 
                          {repr_attr = repr.repr_attr;
                            repr_charsize = repr.repr_charsize;
                            repr_line_pos = repr.repr_line_pos+len;
                            repr_line_len = repr.repr_line_len - len;
                            repr_size = repr.repr_charsize * (repr.repr_line_len - len);
                            repr_pos = repr.repr_pos + (len * repr.repr_charsize)
                          }
                        ], (
                          { 
                             repr_attr = repr.repr_attr;
                            repr_charsize = repr.repr_charsize;
                            repr_line_pos = repr.repr_line_pos;
                            repr_pos = repr.repr_pos;
                            repr_line_len = len;
                            repr_size = repr.repr_charsize * len;
                          }
                            :: list_r)
                    in
                    List.iter 
                      (fun repr ->
                        repr.repr_attr <- repr.repr_attr lor (1 lsl 24))
                    after;
                    before @ after)
            in
            line.representation <- iter line.representation []
          end;
    
    end;
  line


let point_to_eol tree point =
  let text = tree.tree_text in    
  low_distance text point.point 
    (text.text_newlines.(point.point_y + 1).position - 1)

let point_to_bol tree point =
  let text = tree.tree_text in    
  low_distance text 
    text.text_newlines.(point.point_y).position
    point.point

let point_to_eof tree point =
  let text = tree.tree_text in    
  low_distance text point.point text.text_size

let point_to_bof tree point =
  let text = tree.tree_text in    
  low_distance text 0 point.point

let move_res text point n =
  if n > 0 then
    fmove_res text point n
  else
    bmove_res text point (-n)

let move text point n = let _ = move_res text point n in ()

let point_to_lof text point n =
  if n > 0 then
    point_to_eof text point
  else
    point_to_bof text point

let point_to_lol text point n =
  if n > 0 then
    point_to_eol text point
  else
    point_to_bol text point

let point_to_line tree point line =
  let text = tree.tree_text in    
  let pos = 
    if text.text_nlines <= line + 1 then text.text_size
    else
      text.text_newlines.(line).position
  in
  move_point_to tree point text.text_newlines.(line).position

let clear tree =
  let text = tree.tree_text in      
  let _ = low_delete tree 0 (text.text_size - text.text_gsize) in
  text.text_history <- [];
  List.iter (fun p -> p.point <- 0; p.point_y <- 0) text.text_points


let point_line text point = point.point_y


let goto_line tree point y =
  let text = tree.tree_text in      
  if text.text_nlines - 1 <= y then  
    set_position tree point (size tree)
  else 
  let line = text.text_newlines.(y) in
  point.point <- line.position;
  point.point_y <- y


let rec region tree p1 p2 =
  let text = tree.tree_text in    
  if p1>p2 then region tree p2 p1
  else
    sub tree p1 (distance tree p1 p2)

let goto_xy tree point x y =
  let text = tree.tree_text in    
  let y =
    if y < text.text_nlines then y
    else text.text_nlines - 1
  in
  point.point <- text.text_newlines.(y).position;
  point.point_y <- y;
  let _ = fmove tree point x in ()

let update tree str =
  let text = tree.tree_text in    
  let newlines = compute_newlines str in
  let len = String.length str in
  let pts = List.map (fun point -> 
        point.point <- get_position tree point) text.text_points
  in
  text.text_string <- str;
  text.text_attrs <- (Array.create len direct_attr);
  text.text_size <- len;
  text.text_gpoint <- 0;
  text.text_gline <- 0;
  text.text_gsize <- 0;
  text.text_newlines <- newlines;
  text.text_nlines <- Array.length newlines;
  text.text_modified <- text.text_modified + 1 ;
  text.text_clean <- true;
  text.text_history <- [];
  List.iter (fun point -> 
      let pos = point.point in
      point.point <- 0;
      point.point_y <- 0;
      set_position tree point pos) 
  text.text_points

let lexing tree curseur end_point =
  let text = tree.tree_text in    
  clean_text text;
  Lexing.from_function 
    (fun str len ->
      let len = min len (distance tree curseur end_point) in
      let len = blit str tree curseur len in
      let _ = fmove tree curseur len in
      len
  )

let start_session tree =   
  let text = tree.tree_text in    
  text.text_modified
  
let commit_session tree session_date =
  let text = tree.tree_text in      
  if text.text_modified > session_date then
    let rec iter session history =
      match history with
        [] -> assert false
      | action :: history ->
          let date =
            match action with
              Session _ -> failwith "Can not commit nested sessions"
            | Insertion (_,_,date) -> date
            | Deletion (_,_,date) -> date
          in
          if date = session_date then
            text.text_history <- (Session (List.rev (action::session)))
            :: history
          else
            iter (action::session) history
    in
    iter [] text.text_history
    
let readonly tree = 
  let text = tree.tree_text in    
  text.text_readonly
  
let toggle_readonly tree = 
  let text = tree.tree_text in    
  text.text_readonly <- not text.text_readonly
  
