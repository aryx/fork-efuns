(*s: core/error.ml *)
open Common

(*s: function [[Efuns.error]] *)
let error_exn s exn =
  let bt = Printexc.get_backtrace () in
  (* less: this introduces a dependency to pfff, but allows for
   * better reporting. An alternative solution would be to register
   * exn handlers/formatters
   *)
  (match exn with 
  | Parsing_error.Lexical_error _ | Parsing_error.Syntax_error _ ->
     let ex = Exception.catch exn in
     (* TODO
     let err = Error_code.exception_to_error "XXX" ex in
     pr2 (Error_code.string_of_error err)
      *)
     pr2 (Dumper.dump ex)
  | _ -> ()
  );
  pr2 (spf "error: %s (exn = %s). backtrace:\n%s" 
        s (Common.exn_to_s exn) bt);
  flush stderr
(*e: function [[Efuns.error]] *)

let error s =
  pr2 (spf "error: %s" s);
  flush stderr
(*e: core/error.ml *)
