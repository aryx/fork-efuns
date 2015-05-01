open Efuns

(* 
 * For really pad specific stuff.
 * Hopefully it will be shorter than my .emacs ...
 *)

let _ =
  Hook.add_start_hook (fun () ->

    (* special functions *)

    Keymap.define_interactive_action "gtd" (fun frame ->
      Frame.load_file frame.frm_window "/home/pad/mobile/GTD/gtd.org" |> ignore
    );

    (* ~/.login like *)
    Unix.putenv "PFFF_HOME" "/home/pad/pfff";
    Unix.putenv "PATH" (
       "/home/pad/.opam/4.01.0/bin:" ^
        (try (Unix.getenv "PATH") with Not_found -> "/bin") ^ 
       ":/home/pad/bin"
     );

    (* projects *)

    
  )
