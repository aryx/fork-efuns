
type level = int (* 1 to 10 *)
type outline_points = (level * Text.point) list

(* to be set by major modes as a buffer local variable *)
val outline_var: outline_points Var.t

(* will use the defined outline_var to outline the file *)
val outline_num: Efuns.action

