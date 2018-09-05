{
(***********************************************************************)
(*                                                                     *)
(*                           The V6 Engine                             *)
(*                                                                     *)
(*          Francois Rouaix, projet Cristal, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: lexpath.mll,v 1.2 1999/09/20 15:19:29 lefessan Exp $ *)
}

rule f  = parse
    [ ^ '/']+ '/'
      	{ (fun l ->
	    let newl = match Lexing.lexeme lexbuf with
	       "./" -> l
	     | "../" -> (match l with [] -> [] | _ :: tl -> tl)
	     | p ->  (String.sub p 0 (String.length p - 1)) :: l
	     in
	    f lexbuf newl)
      	}
  | "/" { (fun l -> f lexbuf l) }
  |  [ ^ '/']+
      	{ (fun l -> 
	     match Lexing.lexeme lexbuf with
	       "." -> l
             | ".." -> (match l with [] -> [] | _ :: tl -> tl)
             | p -> p :: l )}
  | "" {(fun l -> l)}

{
 let rev_path_components s = f (Lexing.from_string s) []

 let path_components s = List.rev (f (Lexing.from_string s) [])

 (* Build a unix path from path components *)
 let rec build s = function
     [] -> s
   | x::l -> build (Filename.concat s x) l

 let remove_dots s =
    build "/" (path_components s)

}
