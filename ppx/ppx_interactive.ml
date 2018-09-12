(* Yoann Padioleau
 *
 * Copyright (C) 2018 Yoann Padioleau
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
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A ppx rewriter to automatically transform 
 *  let foo frm = ... [@@interactive]
 * into
 *  let foo frm = ... let _ = Action.define_action "foo" foo
 * 
 * Usage to test:
 *   $ ocamlfind ppx_tools/rewriter ./ppx_interactive tests/test_interactive.ml
 * 
 * To get familiar with the OCaml AST you can use:
 *   $ ocamlfind ppx_tools/dumpast tests/test_interactive.ml
 * 
 * Here is its output on tests/test_interactive.ml:
 *   ==>
 *   [{pstr_desc =
 *      Pstr_value (Nonrecursive,
 *       [{pvb_pat = {ppat_desc = Ppat_var {txt = "foo"}};
 *         pvb_expr =
 *          {pexp_desc =
 *            Pexp_fun ("", None, {ppat_desc = Ppat_var {txt = "frame"}},
 *             {pexp_desc =
 *               Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "failwith"}},
 *                [("", {pexp_desc = Pexp_constant (Const_string ("TODO", None))})])})};
 *         pvb_attributes = [({txt = "interactive"}, PStr [])]}])}]
 *   =========
 * (I wish I could use ~/pfff/pfff -dump_ml, but my AST is different).
 * 
 * doc:
 *  - original tutorial blog post for ppx_getenv:
 *  https://whitequark.org/blog/2014/04/16/a-guide-to-extension-points-in-ocaml/
 *  - update of ppx_getenv using the latest ppxlib
 *  http://rgrinberg.com/posts/extension-points-3-years-later/
 *  (in my opinion it's not worth the complexity)
 *)

(*****************************************************************************)
(* Mapper *)
(*****************************************************************************)

let mapper argv =
  { default_mapper with
    structure = fun mapper xs ->
      xs |> List.map (fun item ->
        match item with
        | { pstr_desc = 
              Pstr_value (Nonrecursive,
                [{pvb_pat = {ppat_desc = Ppat_var {txt = fname; _}; _};
                  pvb_attributes = [({txt = "interactive"; loc}, PStr args)]; _}
                ])
          ; _} -> 
          let action_name =
            match args with
            | [] -> fname
            | [{pstr_desc =
                Pstr_eval
                  ({pexp_desc = Pexp_constant (Const_string (name, None));_},
                   _); _}] -> name
            | _ -> 
              raise (Location.Error (
                Location.error ~loc 
                  "@@interactive accepts nothing or a string"))
          in
          let action = 
            Str.eval 
              (Exp.apply 
                 (Exp.ident 
                    {txt = Ldot (Lident "Action", "define_action" ); loc})
                 ["", Exp.constant (Const_string (action_name, None));
                  "", Exp.ident {txt = Lident fname; loc};
                 ])
          in
          [ item; action]
        | x -> [default_mapper.structure_item mapper x]
      ) |> List.concat
  }

let () = register "interactive" mapper
