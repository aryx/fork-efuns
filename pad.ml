open Efuns

(* 
 * This file contains hopefully all the pad-specific keybindings/cmds/options.
 * Hopefully it will be shorter than my .emacs ...
 * alt: have a .efunsrc, but not everything is customizable through options.
 *)

let _ =
  Hook.add_start_hook (fun () ->

    (* keybindings *)
    [
      [MetaMap, Char.code 'g'], Complexe.goto_line;
      [MetaMap, XK.xk_Up], Scroll.scroll_up; 
      [MetaMap, XK.xk_Down], Scroll.scroll_down; 
      [MetaMap, XK.xk_Left], Scroll.scroll_other_up; 
      [MetaMap, XK.xk_Right], Scroll.scroll_other_down; 

      [ControlMap, Char.code '/'], Edit.undo;

      [MetaMap, XK.xk_Return], Compil.compile;
      [ControlMap, Char.code 'n' ], Compil.next_error;

      [MetaMap, Char.code '1'], Shell.eshell_num;
      [MetaMap, Char.code '2'], Shell.eshell_num;
      [MetaMap, Char.code '3'], Shell.eshell_num;
      [MetaMap, Char.code '4'], Shell.eshell_num;
      [MetaMap, Char.code '5'], Shell.eshell_num;
      [MetaMap, Char.code '9'], Shell.eshell_num;
      [MetaMap, Char.code '0'], Shell.eshell_num;
      [MetaMap, Char.code '-'], Shell.eshell_num;
      [MetaMap, Char.code '='], Shell.eshell_num;

      [ControlMetaMap, XK.xk_Tab], Buffer_menu.menu;
      [ControlMetaMap, Char.code 'l'], Multi_buffers.switch_to_other_buffer;

      (* lefessant? in emacs now? *)
      [ControlMetaMap, XK.xk_Left], Multi_buffers.left_buffer;
      [ControlMetaMap, XK.xk_Right], Multi_buffers.right_buffer;

      (* pad: xemacs inspired *)
      [ControlMap, XK.xk_Tab], Multi_frames.next_frame;

    ] |> List.iter (fun (keys, action) ->
      Keymap.add_global_key keys "TODO" action
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
