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
open Ppxlib
open Ast_helper

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
 *  - update to use ocaml-migrate-parsetree so portable ppx rewriter
 *   http://ocamllabs.io/projects/2017/02/15/ocaml-migrate-parsetree.html
 *  - update: now use ppxlib instead of ocaml-migrate-parsetree, see
 *    ppx_profiling.ml in pfff for more information
 *)

(*****************************************************************************)
(* Mapper *)
(*****************************************************************************)

(* TODO: like in ppx_profiling.ml, use Ast_traverse *)
let impl xs =
      xs |> List.map (fun item ->
        match item with
        (* let <fname> = ... [@@interactive <args_opt> *)
        | { pstr_desc = 
              Pstr_value (Nonrecursive,
                [{pvb_pat = {ppat_desc = Ppat_var {txt = fname; _}; _};
                  pvb_attributes = [
                          { attr_name = {txt = "interactive"; loc};
                            attr_payload = PStr args; attr_loc = _;
                          }
                    ];
                  pvb_loc = _;
                  pvb_expr = _body;
                 }
                ]);
                _} -> 
          (* you can change the action name by specifying an explicit name
           * with [@@interactive "<explicit_name>"]
           *)
          let action_name =
            match args with
            | [] -> fname
            | [{pstr_desc =
                Pstr_eval
                  ({pexp_desc = Pexp_constant (Pconst_string (name, _loc, None));_},
                   _); _}] -> name
            | _ -> 
              Location.raise_errorf ~loc 
                  "@@interactive accepts nothing or a string"
          in
          (* let _ = Action.define_action <action_name> <fname> *)
          let action = 
            Str.eval 
              (Exp.apply 
                 (Exp.ident 
                    {txt = Ldot (Lident "Action", "define_action" ); loc})
                 [Nolabel, Exp.constant (Pconst_string (action_name, loc, None));
                  Nolabel, Exp.ident {txt = Lident fname; loc};
                 ])
          in
          [ item; action]
        | x -> [x]
      ) |> List.concat

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let () = 
  Driver.register_transformation ~impl "ppx_interactive"
