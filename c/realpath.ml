external c_realpath: string -> string option = "caml_realpath"

let realpath x = c_realpath x

(*
let realpath2 path =
  match c_realpath path with
  | Some s -> s
  | None -> failwith (spf "problem with realpath on %s" path)

let realpath path =
  profile_code "Common.realpath" (fun () -> realpath2 path)

 *)
