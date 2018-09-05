open Common

let test () =
  let clipboard = GData.clipboard Gdk.Atom.clipboard in
  (* if you type Tab after the # it should complete *)
  clipboard#
