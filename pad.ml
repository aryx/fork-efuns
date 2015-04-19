open Efuns

(* for really pad specific stuff *)

let _ =
  Hook.add_start_hook (fun () ->
    Keymap.define_interactive_action "gtd" (fun frame ->
      Frame.load_file frame.frm_window "/home/pad/mobile/GTD/gtd.org" |> ignore
    )  
  )