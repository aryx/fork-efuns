(*s: core/text.ml *)
(*s: copyright header2 *)
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
(*e: copyright header2 *)
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
(*
open WX_types
open WX_text
*)
(* from WX_types *)
type font = string
type color = string

(*s: type Text.item *)
type item =
  String     of item_attr list * int
| RealString of item_attr list * string
(*e: type Text.item *)
(*s: type Text.item_attr *)
and item_attr =
  Font of font
| Foreground of color
| Background of color
(*e: type Text.item_attr *)


(*s: type Text.position *)
type position = int
(*e: type Text.position *)
(*s: type Text.version *)
type version = int
(*e: type Text.version *)

(*s: type Text.attribute *)
type attribute = int
(*e: type Text.attribute *)
(*s: type Text.delta *)
type delta = int
(*e: type Text.delta *)
(*s: type Text.session *)
type session = int
(*e: type Text.session *)



(*s: type Text.line *)
and line = {
    mutable position : position; (* bol (beginning of line) *)

    (*s: [[Text.line]] representation fields *)
    mutable representation : repr list;
    mutable repr_len : int;
    (*x: [[Text.line]] representation fields *)
    mutable repr_string : string;
    (*e: [[Text.line]] representation fields *)
    (*s: [[Text.line]] attribute fields *)
    (* hightlighting on the line:
    0 => no hightlighting
    n => chars are hightlighted from the beginning of the line until pos n
    *)
    mutable items : item array;
    (*e: [[Text.line]] attribute fields *)
    (*s: [[Text.line]] other fields *)
    mutable line_modified : int; (* first modified position *)
    (*x: [[Text.line]] other fields *)
    mutable line_hlt : int; (* highlighting *)
    (*e: [[Text.line]] other fields *)
  } 
(*e: type Text.line *)

(*s: type Text.point *)
and point = {
    mutable pos : position;
    mutable line : int;
  }
(*e: type Text.point *)

(*s: type Text.repr *)
and repr = 
  { 
    repr_line_pos : position;   (* pos of repr in Text.t string *)
    repr_line_len : int;   (* len of repr in Text.t string *)
    
    repr_pos : int;  (* pos of repr in representation string *)
    repr_size : int;

    mutable repr_attr : int;    (* common attribute *)
    repr_charsize : int; (* common size *)
  } 
(*e: type Text.repr *)

(*s: type Text.text *)
type text = {
    mutable text_string : string;
    mutable text_size : int; (* String.length text.text_string *)

    mutable text_newlines : line array;
    mutable text_nlines : int; (* Array.length text.text_newlines *)
    (*s: [[Text.text]] gap fields *)
    (* g for gap *)
    mutable gpoint : point;
    mutable gsize : int;
    (*e: [[Text.text]] gap fields *)
    (*s: [[Text.text]] history fields *)
    mutable text_modified : version;
    (*x: [[Text.text]] history fields *)
    mutable text_history : action list;
    (*e: [[Text.text]] history fields *)
    (*s: [[Text.text]] attribute fields *)
    mutable text_attrs : attribute array;
    (*e: [[Text.text]] attribute fields *)
    (*s: [[Text.text]] other fields *)
    mutable text_points : point list;
    (*x: [[Text.text]] other fields *)
    mutable text_clean : bool;
    (*x: [[Text.text]] other fields *)
    mutable text_readonly : bool;
    (*e: [[Text.text]] other fields *)
  } 
(*e: type Text.text *)
  
(*s: type Text.action *)
and action =
  Insertion of int * int * int
| Deletion of int * string * int
(*s: [[Text.action]] other cases *)
| Session of action list
(*e: [[Text.action]] other cases *)
(*e: type Text.action *)


(*      
module TextTree = WX_text.Make(Text)
open TextTree
*)
type tree_desc =
      { mutable tree_nlines: int;
        mutable tree_width: int;
        mutable tree_height: int;
(*        mutable tree_parts: 'a array; *)
(*        mutable tree_up: tree tree_desc; *)
        mutable tree_pos: int;
        mutable tree_modified: bool;
        mutable line_height: int;
        mutable line_width: int;

        mutable tree_text: text 
}

let make_text text lines =
  let nlines = Array.length lines in
  let tree = {
      tree_nlines = nlines;
      tree_width = 0;
      tree_height = 0;
(*
      tree_parts = [| Lines {
          tree_nlines = nlines;
          tree_width = 0;
          tree_height = 0;
          tree_parts = lines;
          tree_up = tree;
          tree_pos = 0;
          tree_modified = true;
          line_height = 0;
          line_width = 0;
          tree_text = text;      
        }|];
      tree_up = tree;
*)
      tree_pos = 0;
      tree_modified = true;
      line_height = 0;
      line_width = 0;
      tree_text = text;
    }
  in tree

type t = tree_desc
  
(* type t = tree tree_desc   *)

(* external id: t -> tree tree_desc = "%identity" *)

(*
let print msg text =
  let s = text.text_string in
  let gpoint = text.gpoint in
  let gsize  = text.gsize in
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

  
(*s: function Text.version *)
let version tree = 
  let text = tree.tree_text in
  text.text_modified
(*e: function Text.version *)
  
(*s: function Text.nbre_lines *)
let nbre_lines tree = 
  let text = tree.tree_text in
  text.text_nlines - 2
(*e: function Text.nbre_lines *)
  
(*s: function Text.size *)
let size tree = 
  let text = tree.tree_text in
  text.text_size - text.gsize
(*e: function Text.size *)
  
(*s: function Text.point_col *)
let point_col tree point = 
  let text = tree.tree_text in    
  let pos = point.pos in
  let gpos = text.gpoint.pos in
  let bol = text.text_newlines.(point.line).position in
  (* gap handling *)
  if bol <= gpos && gpos < pos
  then pos - bol - text.gsize
  else pos - bol
(*e: function Text.point_col *)

(*s: function Text.make_attr *)
let make_attr fg bg font highlighted =
  let attr = 
     fg + 
     (bg lsl 8) + 
     (font lsl 16) + 
     (if highlighted then 1 lsl 24 else 0)
  in
  attr
(*e: function Text.make_attr *)

(*s: constant Text.direct_attr *)
let direct_attr =  make_attr 0 1 0 false
(*e: constant Text.direct_attr *)
(*s: constant Text.inverse_attr *)
let inverse_attr =  make_attr 1 0 0 false
(*e: constant Text.inverse_attr *)

(*s: function Text.mk_line_with_pos *)
let mk_line_with_pos pos = 
  {
   position = pos; 

   representation = []; 
   repr_len = 0; 
   repr_string = ""; 

   line_modified = 0; 
   line_hlt = 0; 
   items = [||]; 
  }
(*e: function Text.mk_line_with_pos *)


(*s: function Text.move_gpoint_to *)
let move_gpoint_to text pos =
  let gpos = text.gpoint.pos in
  let gsize = text.gsize in
  let gline = text.gpoint.line in
  let gap_end = gpos + gsize in

  text.text_clean <- false;
  if pos <> gpos then
    if pos < gpos then begin
      (*s: [[Text.move_gpoint_to()]] when pos is before gpos *)
      let delta = gpos - pos in
      let (delta_line,_) = 
        Utils.count_char_sub text.text_string pos delta '\n' in
      String.blit  text.text_string pos   text.text_string (pos + gsize)   delta;
      Array.blit   text.text_attrs  pos   text.text_attrs  (pos + gsize)   delta;
      for i = gline - delta_line + 1 to gline do
        text.text_newlines.(i).position
          <- text.text_newlines.(i).position + gsize 
      done;
      text.text_points |> List.iter (fun p -> 
        if p.pos > pos && p.pos <= gpos 
        then p.pos <- p.pos + gsize
      );
      text.gpoint <- { pos = pos; line = gline - delta_line };
      (*e: [[Text.move_gpoint_to()]] when pos is before gpos *)
    end else begin
      (*s: [[Text.move_gpoint_to()]] when pos is after gpos *)
      let delta = pos - gap_end in
      let (delta_line,_) = 
        Utils.count_char_sub text.text_string gap_end delta '\n' in
      String.blit  text.text_string gap_end   text.text_string gpos   delta;
      Array.blit   text.text_attrs  gap_end   text.text_attrs  gpos   delta;
      for i = gline + 1 to gline + delta_line do
        text.text_newlines.(i).position
          <- text.text_newlines.(i).position - gsize
      done;
      text.text_points |> List.iter (fun p -> 
          if p.pos >= gap_end && p.pos <= pos 
          then p.pos <- p.pos - gsize
      );
      text.gpoint <- { pos = pos - gsize; line = gline + delta_line }
      (*e: [[Text.move_gpoint_to()]] when pos is after gpos *)
    end
(*e: function Text.move_gpoint_to *)

(*s: function Text.cancel_repr *)
let cancel_repr text point n =
  let line = text.text_newlines.(n) in
  let pos =  point - line.position in
  line.line_modified <- 
    (if line.line_modified < 0
    then pos
    else min line.line_modified pos
    )
(*e: function Text.cancel_repr *)

(*s: constant Text.add_amount *)
let add_amount = define_option ["add_amount"] "Size of the gap in the buffer"
  int_option 200
(*e: constant Text.add_amount *)
(*s: function Text.extend_gap *)
let extend_gap text amount =
  let add_size = max !!add_amount 
      ((amount / !!add_amount) * !!add_amount + !!add_amount) in
  let old_size = text.text_size in
  let gsize = text.gsize in

  let gpos = text.gpoint.pos in
  let gap_end = gpos + gsize in

  let new_text = String.create (old_size + add_size) in
  String.blit  text.text_string 0   new_text 0    gpos; 
  String.blit  text.text_string gap_end   new_text (gap_end + add_size) 
    (old_size - gap_end);
  text.text_string <- new_text;

  let new_attrs = Array.create (old_size + add_size) direct_attr in
  Array.blit   text.text_attrs 0   new_attrs 0   gpos; 
  Array.blit   text.text_attrs gap_end    new_attrs (gap_end + add_size) 
    (old_size - gap_end);
  text.text_attrs <- new_attrs;

  for i = text.gpoint.line + 1 to text.text_nlines - 1 do
    text.text_newlines.(i).position <- 
      text.text_newlines.(i).position + add_size
  done;
  text.text_points |> List.iter (fun p -> 
      if p.pos > gpos 
      then p.pos <- p.pos + add_size
  );
  text.gsize <- gsize + add_size;
  text.text_size <- old_size + add_size;
  ()
(*e: function Text.extend_gap *)

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

(*s: function Text.low_insert *)
let low_insert tree pos str =
  let text = tree.tree_text in
  let strlen = String.length str in
  (*s: [[Text.low_insert()]] fail if readonly buffer *)
  if text.text_readonly 
  then failwith "Buffer is read-only";
  (*e: [[Text.low_insert()]] fail if readonly buffer *)

  (*s: [[Text.low_insert()]] move gap to point *)
  move_gpoint_to text pos;
  (*e: [[Text.low_insert()]] move gap to point *)
  (* subtle: don't move those 'let' earlier, because moving the gap do side
   * effects on the position and line of the gpoint. 
   *)
  let gpos = text.gpoint.pos in
  let gline = text.gpoint.line in

  cancel_repr text gpos gline;
  (*s: [[Text.low_insert()]] extend gap if not enough space *)
  if strlen > text.gsize 
  then extend_gap text strlen;
  (*e: [[Text.low_insert()]] extend gap if not enough space *)
  (* all points should have their point.pos correctly adjusted *)

  String.blit  str 0   text.text_string gpos    strlen;
  (* todo: should refontify! *)
  Array.fill text.text_attrs gpos strlen direct_attr;

  let (nbr_newlines, _nbr_chars) = Utils.count_char str '\n' in
  if nbr_newlines > 0 then 
  begin
    (*s: [[Text.low_insert()]] adjust newlines when str contains newlines *)
    if (Array.length text.text_newlines - text.text_nlines) < nbr_newlines then
      begin
        (*s: [[Text.low_insert()]] grow newlines *)
        let old_size = text.text_nlines in
        let new_cache = Array.create (old_size + (max 20 nbr_newlines)) 
          { (mk_line_with_pos (-1)) with line_modified = -1 }
        in
        Array.blit 
          text.text_newlines 0 
          new_cache 0 
         old_size;
        text.text_newlines <- new_cache;
        (*e: [[Text.low_insert()]] grow newlines *)
      end;

    Array.blit
      text.text_newlines (gline+1) 
      text.text_newlines (gline+1+ nbr_newlines) 
     (text.text_nlines - gline -1);
    text.text_nlines <- text.text_nlines + nbr_newlines;

    (* similar to compute_newlines() but just for lines after gpos *)
    let rec iter n pos =
      let new_pos = String.index_from text.text_string pos '\n' in
      text.text_newlines.(gline+n) <- mk_line_with_pos (new_pos + 1);
      if n < nbr_newlines 
      then iter (n+1) (new_pos + 1)
    in
    iter 1 gpos;
    (*e: [[Text.low_insert()]] adjust newlines when str contains newlines *)
  end;
  let gline = text.gpoint.line in
  text.text_points |> List.iter (fun p ->
    if p.pos > gpos then
      (* todo? why this extra condition?? BUG?? unit test? *)
      if p.line = gline 
      then p.line <- p.line + nbr_newlines;

  );
  text.gpoint <- { pos = gpos + strlen; line = gline + nbr_newlines };
  text.gsize <- text.gsize - strlen;
  (gpos, strlen, text.text_modified) 
(*e: function Text.low_insert *)

(*s: function Text.low_delete *)
let low_delete tree pos len =
  let text = tree.tree_text in      
  (*s: [[Text.low_insert()]] fail if readonly buffer *)
  if text.text_readonly 
  then failwith "Buffer is read-only";
  (*e: [[Text.low_insert()]] fail if readonly buffer *)

  (*s: [[Text.low_insert()]] move gap to point *)
  move_gpoint_to text pos;
  (*e: [[Text.low_insert()]] move gap to point *)
  (* subtle: don't move those 'let' earlier, because moving the gap do side
   * effects on the position and line of the gpoint. 
   *)
  let gpos = text.gpoint.pos in
  let gline = text.gpoint.line in

  cancel_repr text gpos gline;

  let gap_end = gpos + text.gsize in
  let len = min (text.text_size - gap_end) len in

  let str = String.sub text.text_string gap_end len in

  let (nbr_newlines, _nbr_chars) = Utils.count_char str '\n' in
  if nbr_newlines > 0 then 
  begin
      (*s: [[Text.low_delete()]] adjust newlines when str contained newlines *)
      Array.blit 
        text.text_newlines (gline + nbr_newlines + 1)
        text.text_newlines (gline + 1) 
       (text.text_nlines - gline - nbr_newlines - 1);
      text.text_nlines <- text.text_nlines - nbr_newlines;
      (*e: [[Text.low_delete()]] adjust newlines when str contained newlines *)
  end;
  text.text_points |> List.iter (fun p -> 
      if p.pos > gap_end + len 
      then p.line <- p.line - nbr_newlines
      else
        (* points that were in deleted region *)
        if p.pos > gpos then begin
           p.pos <- gpos;
           p.line <- gline
         end
  );
  text.gsize <- text.gsize + len;
  (gpos, str, text.text_modified) 
(*e: function Text.low_delete *)

(*s: function Text.undo *)
let undo tree =
  let text = tree.tree_text in  
  let rec undo action =
    let gpos = text.gpoint.pos in
    let gsize = text.gsize in
    match action with
    | Insertion(point_pos, len, modified) ->
        let point = if gpos < point_pos then point_pos + gsize else point_pos in
        let (pos,str,modif) = low_delete tree point len in
        text.text_modified <- modified;
        Deletion(pos,str,modif), point_pos, 0
    | Deletion (point_pos, str, modified) ->
        let point = if gpos < point_pos then point_pos + gsize else point_pos in
        let (pos,len,modif) = low_insert tree point str in
        text.text_modified <- modified;
        Insertion(pos,len,modif), point_pos, String.length str
    (*s: [[Text.undo()]] match action cases *)
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
    (*e: [[Text.undo()]] match action cases *)
  in
  match text.text_history with
    [] -> raise Not_found
  | action :: tail -> 
      let rev_action = undo action in
      text.text_history <- tail;
      rev_action
(*e: function Text.undo *)

(*s: function Text.insert_at_end *)
let insert_at_end tree str =
  let text = tree.tree_text in
  low_insert tree text.text_size str |> ignore;
  text.text_history <- [];
  text.text_modified <- text.text_modified + 1
(*e: function Text.insert_at_end *)

(*s: function Text.insert_res *)
let insert_res tree point str =
  let text = tree.tree_text in
  let (pos,len,modif) = low_insert tree point.pos str in
  text.text_history <- Insertion(pos,len,modif) :: text.text_history;
  text.text_modified <- text.text_modified + 1;
  pos, len
(*e: function Text.insert_res *)

(*s: function Text.insert *)
let insert text point str = 
   insert_res text point str |> ignore
(*e: function Text.insert *)
  
(*s: function Text.delete_res *)
let delete_res tree point len =
  let text = tree.tree_text in  
  let (pos,str,modif) = low_delete tree point.pos len in
  text.text_history <- Deletion(pos,str,modif) :: text.text_history;
  text.text_modified <- text.text_modified + 1;
  pos, str
(*e: function Text.delete_res *)

(*s: function Text.delete *)
let delete text point len = 
  delete_res text point len |> ignore
(*e: function Text.delete *)

  
(*s: function Text.compute_newlines *)
let compute_newlines string =
  let (nbr_newlines,_) = Utils.count_char string '\n' in
  let newlines = Array.create (nbr_newlines + 2) (mk_line_with_pos 0) in
  let curs = ref 0 in
  (* newlines.(0) is already set with 0 position for its bol *)
  for i = 1 to nbr_newlines do
    let pos = String.index_from string !curs '\n' in
    newlines.(i) <- mk_line_with_pos (pos+1);
    curs := pos + 1;
  done;
  newlines.(nbr_newlines+1) <- mk_line_with_pos (String.length string + 1);
  newlines
(*e: function Text.compute_newlines *)

(*s: function Text.create *)
let create str =
  let newlines = compute_newlines str in
  let attrs = (Array.create (String.length str) direct_attr) in

  let text =
    {
      text_string = str;
      text_size = String.length str;

      text_newlines = newlines;
      text_nlines = Array.length newlines;

      text_attrs = attrs;

      text_points = [];
      (* no gap at the beginning *)
      gpoint = { pos = 0; line = 0 };
      gsize = 0;

      text_modified = 0;
      text_clean = false;
      text_history = [];
      text_readonly = false;
    }
  in
  make_text text (Array.copy newlines)
(*e: function Text.create *)
  
(*s: function Text.find_xy *)
let find_xy text point line pos =
  let text = text.tree_text in    
  let gpos = text.gpoint.pos in
  let gline = text.gpoint.line in
  let gap_end = gpos + text.gsize in
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
        let gchars = gpos - text.text_newlines.(gline).position in
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
      if point < gpos && pos <= point then
        iter line 
      else
        iter gline in
    line, pos - text.text_newlines.(line).position
  in
  x,y
(*e: function Text.find_xy *)

(*s: function Text.new_point *)
let new_point tree =
  let text = tree.tree_text in      
  let p = { pos = 0; line = 0; } in    
  text.text_points <- p :: text.text_points;
  p
(*e: function Text.new_point *)

(*s: function Text.dup_point *)
let dup_point tree point =
  let text = tree.tree_text in      
  let p = {
      pos = point.pos;
      line = point.line
    } in
  text.text_points <- p :: text.text_points;
  p
(*e: function Text.dup_point *)

(*s: function Text.goto_point *)
let goto_point _text p1 p2 =
  p1.pos <- p2.pos;
  p1.line <- p2.line
(*e: function Text.goto_point *)

(*s: function Text.move_point_to *)
let move_point_to tree point p =
  let text = tree.tree_text in    
  let _x,y = find_xy tree text.gpoint.pos text.gpoint.line p in
  point.pos <- p;
  point.line <- y
(*e: function Text.move_point_to *)

(*s: function Text.remove_point *)
let remove_point tree p =
  let text = tree.tree_text in      
  text.text_points <- 
    text.text_points |> List.fold_left (fun points point ->
        if point == p then points else point :: points) 
    []
(*e: function Text.remove_point *)

let with_dup_point text point f =
  let p = dup_point text point in
  Common.finalize (fun () -> f p) (fun () -> remove_point text p)


(*s: function Text.read *)
let read inc =
  create (read_string inc)
(*e: function Text.read *)

(*s: function Text.save *)
let save tree outc =
  let text = tree.tree_text in    
  let str = text.text_string in
  let gpos = text.gpoint.pos in
  let gsize = text.gsize in
  output outc str 0 gpos;
  output outc str (gpos + gsize) 
  (text.text_size - gpos - gsize)
(*e: function Text.save *)

(*s: function Text.unset_attr *)
let unset_attr tree =
  let text = tree.tree_text in  
  Array.fill text.text_attrs 0 (Array.length text.text_attrs) direct_attr;
  Array.iter (fun line -> line.line_modified <- 0) text.text_newlines
(*e: function Text.unset_attr *)

(*s: function Text.set_attr *)
let set_attr tree point len attr = (* should not exceed one line *)
  let text = tree.tree_text in  
  if len > 0 then
    let gap_end = text.gpoint.pos + text.gsize in

    let x,y = find_xy tree text.gpoint.pos text.gpoint.line point.pos in
    cancel_repr text point.pos y;

    let pos = point.pos in
    let gpos = text.gpoint.pos in
    let before, after, after_pos =
      if pos > gap_end then
        0, (min (text.text_size - pos) len), pos
      else
      if pos + len <= gpos then
        0, len, pos
      else
      let before = gpos - pos in
      let after = min (len - before) (text.text_size - gap_end) in
      before, after, gap_end
    in
    if before > 0 
    then Array.fill text.text_attrs pos before attr;
    Array.fill text.text_attrs after_pos after attr
(*e: function Text.set_attr *)

(*s: function Text.low_distance *)
let low_distance text p1 p2 =
  if p1 >= p2 then 0 else
  if p1 <= text.gpoint.pos then
    if p2 <= text.gpoint.pos then
      p2 - p1
    else
      p2 - p1 - text.gsize
  else
  if p2 <= text.gpoint.pos then
    p2 - p1 + text.gsize
  else
    p2 - p1
(*e: function Text.low_distance *)

(*s: function Text.distance *)
let distance tree p1 p2 =
  let text = tree.tree_text in    
  low_distance text p1.pos p2.pos
(*e: function Text.distance *)

(*s: function Text.compare *)
let compare text p1 p2 = 
  compare p1.pos p2.pos
(*e: function Text.compare *)
  
(*s: function Text.add *)
(*
let add text point delta =
  let gpos = text.gpoint.pos in
  let gap_end = gpos + text.gsize in
  if point <= gpos && point + delta > gpos then
    point + delta + text.gsize
  else
  if point >= gap_end && point+delta < gap_end then
    point + delta - text.gsize
  else
    point + delta
*)
(*e: function Text.add *)

(*s: function Text.get_char *)
let get_char tree point =
  let text = tree.tree_text in    
  let pos = point.pos in
  let gpos = text.gpoint.pos in
  let pos = 
    if pos = gpos 
    then pos + text.gsize 
    else pos
  in
  if pos < text.text_size 
  then text.text_string.[pos]
  else '\000'
(*e: function Text.get_char *)

(*s: function Text.get_attr *)
let get_attr tree point =
  let text = tree.tree_text in
  let pos = 
    (* gap handling *)
    if point.pos = text.gpoint.pos
    then point.pos + text.gsize 
    else point.pos
  in
  if pos < text.text_size 
  then text.text_attrs.(pos)
  else direct_attr
(*e: function Text.get_attr *)


(*s: function Text.set_char_attr *)
let set_char_attr tree point attr =
  let text = tree.tree_text in    
  let y = point.line in
  let pos = 
    (* gap handling *)
    if point.pos = text.gpoint.pos
    then point.pos + text.gsize 
    else point.pos
  in
  if pos < text.text_size then begin
    cancel_repr text pos y;
    text.text_attrs.(pos) <- attr
  end
(*e: function Text.set_char_attr *)

(*s: function Text.fmove_res *)
let fmove_res tree p delta =
  let text = tree.tree_text in    
  if delta = 0 then 0 else
  let gpos = text.gpoint.pos in
  let size = text.text_size in
  let gap_end = gpos + text.gsize in
  let gline = text.gpoint.line in
  let pos = p.pos in
  let old_pos = pos in
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
  let (y,pos) = 
    if pos + delta <= gpos then
      iter p.line pos (pos+delta)
    else
    if pos >= gap_end then
      let delta = min delta (size - pos) in
      iter p.line pos (pos + delta)
    else
    let delta = min (delta - (gpos - pos)) (size - gap_end) in
    iter gline gap_end (gap_end + delta) 
  in
  p.pos <- pos;
  p.line <- y;
  low_distance text old_pos pos
(*e: function Text.fmove_res *)



(*s: function Text.bmove_res *)
let bmove_res tree p delta =
  let text = tree.tree_text in    
  if delta = 0 then 0 else
  let gpos = text.gpoint.pos in
  let gap_end = gpos + text.gsize in
  let gline = text.gpoint.line in
  let pos = p.pos in
  let old_pos = pos in
  let lines = text.text_newlines in
  let rec iter y end_point =
    let start_line = lines.(y).position in
    if end_point >= start_line then
      (y, end_point)
    else
      iter (y-1) end_point
  in
  let (y,pos) = 
    if pos - delta >= gap_end then
      if pos - delta < lines.(gline+1).position then
        (
          gline, pos - delta)
      else
        iter p.line (pos-delta)
    else
    if pos <= gpos then
      let delta = min delta pos in
      iter p.line (pos - delta)
    else
    let delta = min (delta - (pos - gap_end)) gpos in
    iter gline (gpos - delta) 
  in
  p.pos <- pos;
  p.line <- y;
  low_distance text pos old_pos
(*e: function Text.bmove_res *)

(*s: function Text.bmove *)
let bmove text p delta = 
  bmove_res text p delta |> ignore
(*e: function Text.bmove *)

(*s: function Text.fmove *)
let fmove text p delta = 
  fmove_res text p delta |> ignore
(*e: function Text.fmove *)
  
(*s: function Text.to_string *)
let to_string tree =
  let text = tree.tree_text in    
  let len = text.text_size - text.gsize in
  if len = 0 
  then "" 
  else begin
    let str = String.create len in
    let gpos = text.gpoint.pos in
    let gap_end = gpos + text.gsize in
    String.blit text.text_string 0 str 0 gpos;
    String.blit text.text_string gap_end str gpos (len- gpos);
    str
  end
(*e: function Text.to_string *)


(*s: function Text.clean_text *)
let clean_text text =
  if not text.text_clean then
    let size = text.text_size in
    let gsize = text.gsize in
    let string = text.text_string in
    move_gpoint_to text size;
    String.fill string (size - gsize) gsize '\000';
    text.text_clean <- true
(*e: function Text.clean_text *)


(*s: function Text.blit *)
let blit str tree point len =
  let text = tree.tree_text in      
  let pos = point.pos in
  let len = min len (low_distance text pos text.text_size) in
  let gpos = text.gpoint.pos in
  let gap_end = gpos + text.gsize in
  if pos+len >= gpos && pos < gap_end 
  then clean_text text;
  (try
    String.blit text.text_string pos str 0 len
   with e -> raise e
   );
  len
(*e: function Text.blit *)
  
(*s: function Text.get_position *)
let get_position tree point = 
  let text = tree.tree_text in    
  (* gap handling *)
  if point.pos > text.gpoint.pos 
  then point.pos - text.gsize
  else point.pos
(*e: function Text.get_position *)

(*s: function Text.set_position *)
let set_position tree point pos =
  let text = tree.tree_text in    
  move_point_to tree point
    (if pos > text.gpoint.pos 
     then pos + text.gsize
     else pos
     )
(*e: function Text.set_position *)
    
(*s: function Text.sub *)
let sub text point len =
  let str = String.create len in
  blit str text point len |> ignore;
  str
(*e: function Text.sub *)
    
(*s: function Text.search_forward *)
let search_forward tree regexp point =
  let text = tree.tree_text in    
  let gsize = text.gsize in
  let gap_end = text.gpoint.pos + gsize in
  if point.pos = text.gpoint.pos 
  then point.pos <- gap_end;
  if point.pos < gap_end 
  then clean_text text;
  let gap_end = text.gpoint.pos + gsize in  
  let string = text.text_string in
  let pos = Str.search_forward regexp string point.pos in
  let pos = if pos >= gap_end then pos - gsize else pos in
  set_position tree point pos;
  Str.match_end () - Str.match_beginning ()
(*e: function Text.search_forward *)

(*s: function Text.replace_matched *)
let replace_matched tree repl =
  let text = tree.tree_text in    
  Str2.replace_matched repl text.text_string
(*e: function Text.replace_matched *)
  
(*s: function Text.search_forward_matched *)
let search_forward_matched tree regexp point =
  let text = tree.tree_text in      
  let gsize = text.gsize in
  let gap_end = text.gpoint.pos + gsize in
  if point.pos = text.gpoint.pos 
  then point.pos <- gap_end;
  if point.pos < gap_end 
  then clean_text text;
  let string = text.text_string in
  let pos = Str.search_forward regexp string point.pos in
  let pos = 
    if pos >= text.gpoint.pos + gsize 
    then pos - gsize 
    else pos 
  in
  set_position tree point pos;
  Str.matched_string string
(*e: function Text.search_forward_matched *)

(*s: function Text.search_forward_groups *)
let search_forward_groups tree regexp point groups =
  let text = tree.tree_text in      
  let gsize = text.gsize in
  let gap_end = text.gpoint.pos + gsize in
  if point.pos = text.gpoint.pos 
  then point.pos <- gap_end;
  if point.pos < gap_end 
  then clean_text text;
  let gap_end = text.gpoint.pos + gsize in  
  let string = text.text_string in
  let pos = Str.search_forward regexp string point.pos in
  let pos = if pos >= gap_end then pos - gsize else pos in
  let array = Array.init groups (fun i -> Str.matched_group (i+1) string) in
  set_position tree point pos;
  array
(*e: function Text.search_forward_groups *)

(*s: function Text.search_backward *)
let search_backward tree regexp point =
  let text = tree.tree_text in    
  if point.pos > text.gpoint.pos 
  then clean_text text;
  let string = text.text_string in
  let start_pos =     
    if point.pos > 0 
    then point.pos - 1 
    else raise Not_found 
  in
  let pos =  Str.search_backward regexp string start_pos in
  set_position tree point pos;
  Str.match_end () - Str.match_beginning ()
(*e: function Text.search_backward *)

(*s: function Text.search_backward_groups *)
let search_backward_groups tree regexp point groups =  
  let text = tree.tree_text in    
  if point.pos > text.gpoint.pos 
  then clean_text text;
  let string = text.text_string in
  let start_pos =     
    if point.pos > 0 
    then point.pos - 1 
    else raise Not_found  
  in
  let pos =  Str.search_backward regexp string start_pos in
  let array = Array.init groups (fun i -> Str.matched_group (i+1) string) in
  set_position tree point pos;
  array
(*e: function Text.search_backward_groups *)


(*s: constant Text.repr_string *)
let repr_string = ref ""
(*e: constant Text.repr_string *)
(*s: constant Text.repr_size *)
let repr_size = ref 0
(*e: constant Text.repr_size *)

(*s: constant Text.dummy_line *)
let (dummy_line : line) = 
  {
    position = max_int;
    representation = [];
    line_modified = 0;
    repr_len = 0;
    repr_string = "";
    line_hlt = 0;
    items = [||];
  } 
(*e: constant Text.dummy_line *)
  
(*s: constant Text.tabreprs *)
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
(*e: constant Text.tabreprs *)
      
(*s: function Text.compute_representation *)
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
  if line.line_modified >= 0 then
    begin
      let end_pos = text.text_newlines.(n+1).position - 1 in
      let rec iter repr_list =
        match repr_list with
          repr :: tail ->
            let next_pos = repr.repr_line_pos + repr.repr_line_len in
            if next_pos < line.line_modified then
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
      let gpos = text.gpoint.pos in
      let gsize = text.gsize in
      let char_repr = ref "" in
      let char_size = ref 0 in
      repr_string := line.repr_string;
      repr_size := String.length line.repr_string;
      if !line_curs >= gpos && 
        !line_curs < gpos + gsize then 
        line_curs := !line_curs + gsize;
      while !line_curs < end_pos do
        let charattr = text.text_attrs.(!line_curs) in
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
          if !line_curs = gpos then line_curs := gpos + gsize;
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
      line.line_modified <- -1;
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
(*e: function Text.compute_representation *)


(*s: function Text.point_to_eol *)
let point_to_eol tree point =
  let text = tree.tree_text in    
  low_distance text point.pos
    (text.text_newlines.(point.line + 1).position - 1)
(*e: function Text.point_to_eol *)

(*s: function Text.point_to_bol *)
let point_to_bol tree point =
  let text = tree.tree_text in    
  low_distance text 
    text.text_newlines.(point.line).position
    point.pos
(*e: function Text.point_to_bol *)

(*s: function Text.point_to_eof *)
let point_to_eof tree point =
  let text = tree.tree_text in    
  low_distance text point.pos text.text_size
(*e: function Text.point_to_eof *)

(*s: function Text.point_to_bof *)
let point_to_bof tree point =
  let text = tree.tree_text in    
  low_distance text 0 point.pos
(*e: function Text.point_to_bof *)

(*s: function Text.move_res *)
let move_res text point n =
  if n > 0 
  then fmove_res text point n
  else bmove_res text point (-n)
(*e: function Text.move_res *)

(*s: function Text.move *)
let move text point n = 
  move_res text point n |> ignore
(*e: function Text.move *)

(*s: function Text.point_to_lof *)
let point_to_lof text point n =
  if n > 0 
  then point_to_eof text point
  else point_to_bof text point
(*e: function Text.point_to_lof *)

(*s: function Text.point_to_lol *)
let point_to_lol text point n =
  if n > 0 
  then point_to_eol text point
  else point_to_bol text point
(*e: function Text.point_to_lol *)

(*s: function Text.point_to_line *)
let point_to_line tree point line =
  let text = tree.tree_text in    
  let pos = 
    if text.text_nlines <= line + 1 
    then text.text_size
    else text.text_newlines.(line).position
  in
  move_point_to tree point pos
(*e: function Text.point_to_line *)

(*s: function Text.clear *)
let clear tree =
  let text = tree.tree_text in      
  low_delete tree 0 (text.text_size - text.gsize) |> ignore;
  text.text_history <- [];
  List.iter (fun p -> p.pos <- 0; p.line <- 0) text.text_points
(*e: function Text.clear *)


(*s: function Text.point_line *)
let point_line _text point = 
  point.line
(*e: function Text.point_line *)


(*s: function Text.goto_line *)
let goto_line tree point y =
  let text = tree.tree_text in      
  if text.text_nlines - 1 <= y 
  then set_position tree point (size tree)
  else begin
    let line = text.text_newlines.(y) in
    point.pos <- line.position;
    point.line <- y
  end
(*e: function Text.goto_line *)


(*s: function Text.region *)
let rec region tree p1 p2 =
  if p1>p2 
  then region tree p2 p1
  else sub tree p1 (distance tree p1 p2)
(*e: function Text.region *)

(*s: function Text.goto_xy *)
(*
let goto_xy tree point x y =
  let text = tree.tree_text in    
  let y =
    if y < text.text_nlines then y
    else text.text_nlines - 1
  in
  point.pos <- text.text_newlines.(y).position;
  point.line <- y;
  fmove tree point x |> ignore
*)
(*e: function Text.goto_xy *)

(*s: function Text.update *)
let update tree str =
  let text = tree.tree_text in    
  let newlines = compute_newlines str in
  let len = String.length str in
  text.text_points |> List.iter (fun point -> 
    point.pos <- get_position tree point
  );
  text.text_string <- str;
  text.text_attrs <- (Array.create len direct_attr);
  text.text_size <- len;
  text.gpoint <- { pos = 0; line = 0 };
  text.gsize <- 0;
  text.text_newlines <- newlines;
  text.text_nlines <- Array.length newlines;
  text.text_modified <- text.text_modified + 1 ;
  text.text_clean <- true;
  text.text_history <- [];
  text.text_points |> List.iter (fun point -> 
      let pos = point.pos in
      point.pos <- 0;
      point.line <- 0;
      set_position tree point pos
  ) 
  
(*e: function Text.update *)

(*s: function Text.lexing *)
let lexing tree curseur end_point =
  let text = tree.tree_text in    
  clean_text text;
  Lexing.from_function 
    (fun str len ->
      let len = min len (distance tree curseur end_point) in
      let len = blit str tree curseur len in
      fmove tree curseur len |> ignore;
      len
  )
(*e: function Text.lexing *)

(*s: function Text.start_session *)
let start_session tree =   
  let text = tree.tree_text in    
  text.text_modified
(*e: function Text.start_session *)
  
(*s: function Text.commit_session *)
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
(*e: function Text.commit_session *)
    
(*s: function Text.readonly *)
let readonly tree = 
  let text = tree.tree_text in    
  text.text_readonly
(*e: function Text.readonly *)
  
(*s: function Text.toggle_readonly *)
let toggle_readonly tree = 
  let text = tree.tree_text in    
  text.text_readonly <- not text.text_readonly
(*e: function Text.toggle_readonly *)
  
(*e: core/text.ml *)
