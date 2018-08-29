open Efuns

(* 
 * For really pad-specific stuff.
 * Hopefully it will be shorter than my .emacs ...
 * alt: have a .efunsrc, but not everything is customizable through options.
 *)

let _ =
  Hook.add_start_hook (fun () ->

    (* special functions *)

    Keymap.define_interactive_action "gtd" (fun frm ->
      Frame.load_file frm.frm_window "/home/pad/GTD/GTD-daily.org" |> ignore
    );

    (* ~/.login like *)
    Unix.putenv "PFFF_HOME" "/home/pad/pfff";
    Unix.putenv "PATH" (
       "/home/pad/.opam/4.02.3/bin:" ^
        (try (Unix.getenv "PATH") with Not_found -> "/bin") ^ 
       ":/home/pad/bin"
     );

    (* projects *)
  )
