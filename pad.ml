open Efuns

(* 
 * For really pad specific stuff.
 * Hopefully it will be shorter than my .emacs ...
 *)

let _ =
  Hook.add_start_hook (fun () ->
    Keymap.define_interactive_action "gtd" (fun frame ->
      Frame.load_file frame.frm_window "/home/pad/mobile/GTD/gtd.org" |> ignore
    )  
  )