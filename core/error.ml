(*s: core/error.ml *)
open Common

(*s: function [[Efuns.error]] *)
let error_exn s exn =
  let bt = Printexc.get_backtrace () in
  pr2 (spf "error: %s (exn = %s). backtrace:\n%s" 
        s (Common.exn_to_s exn) bt);
  flush stderr
(*e: function [[Efuns.error]] *)

let error s =
  pr2 (spf "error: %s" s);
  flush stderr
(*e: core/error.ml *)
