(*s: core/error.ml *)
open Common
module PI = Parse_info

(*s: function [[Efuns.error]] *)
let error_exn s exn =
  let bt = Printexc.get_backtrace () in
  (* less: this introduces a dependency to pfff, but allows for
   * better reporting. An alternative solution would be to register
   * exn handlers/formatters
   *)
  (match exn with 
  | PI.Lexical_error _ | PI.Parsing_error _ ->
     let err = Error_code.exn_to_error "XXX" exn in
     pr2 (Error_code.string_of_error err)
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
