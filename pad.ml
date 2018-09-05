open Efuns

(* 
 * This file contains hopefully all the pad-specific commands and options.
 * Hopefully it will be shorter than my .emacs ...
 * alt: have a .efunsrc, but not everything is customizable through options.
 *)

let _ =
  Hook.add_start_hook (fun () ->

    (* keybindings *)
    [
      [MetaMap, Char.code 'g'], "goto_line";
      [MetaMap, XK.xk_Up], "scroll_up"; 
      [MetaMap, XK.xk_Down], "scroll_down"; 
      [MetaMap, XK.xk_Left], "scroll_other_up"; 
      [MetaMap, XK.xk_Right], "scroll_other_down"; 

      [ControlMap, Char.code '/'], "undo";

      [MetaMap, XK.xk_Return], "compile";
      [ControlMap, Char.code 'n' ], "next_error";

      [MetaMap, Char.code '1'], "eshell_num";
      [MetaMap, Char.code '2'], "eshell_num";
      [MetaMap, Char.code '3'], "eshell_num";
      [MetaMap, Char.code '4'], "eshell_num";
      [MetaMap, Char.code '5'], "eshell_num";
      [MetaMap, Char.code '9'], "eshell_num";
      [MetaMap, Char.code '0'], "eshell_num";
      [MetaMap, Char.code '-'], "eshell_num";
      [MetaMap, Char.code '='], "eshell_num";

      [ControlMetaMap, XK.xk_Tab], "buffer_menu";
      [ControlMetaMap, Char.code 'l'], "switch_to_other_buffer";

      (* lefessant? in emacs now? *)
      [ControlMetaMap, XK.xk_Left], "left_buffer";
      [ControlMetaMap, XK.xk_Right], "right_buffer";

      (* pad: xemacs inspired *)
      [ControlMap, XK.xk_Tab], "next_frame";

    ] |> List.iter (fun (keys, action) ->
      Keymap.add_global_key keys 
    );
  
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

    (* Hooks *)
    Hook.add_hook Caml_mode.hooks 
      (Minor_modes.toggle_minor_buffer (Ocaml_merlin.mode));
  )
