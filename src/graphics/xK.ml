
(* must be less than Char.code 'A', the first printable ascii char *)
let xk_Up = 1
let xk_Down = 2
let xk_Left = 3
let xk_Right = 4

let xk_Prior = 5
let xk_Next = 6

let xk_BackSpace = 8
let xk_Tab = 9
(* on some machines it's C-m (13), on some it's C-j (10) *)
let xk_Return = 13



(* must at least not conflict with other characters *)
let xk_Pointer_Button_Dflt = 2000
let xk_Pointer_Button1 = xk_Pointer_Button_Dflt + 1
let xk_Pointer_Button2 = xk_Pointer_Button_Dflt + 2
let xk_Pointer_Button3 = xk_Pointer_Button_Dflt + 3

let xk_Pointer_Button4 = xk_Pointer_Button_Dflt + 4
let xk_Pointer_Button5 = xk_Pointer_Button_Dflt + 5

(* TODO *)
let xk_Pointer_Drag_Dflt = 0
let xk_Pointer_Drag1 = 0

let xk_Menu = 0

let xk_Shift_L = 0
let xk_Hyper_R = 0

let xk_Insert = 0
let xk_Delete = 0

let xk_q = 0

(* for *bindings* to correctly pretty print the binded special keys *)
let keysym_to_name = [
  xk_Up, "Up";
  xk_Down, "Down";
  xk_Left, "Left";
  xk_Right, "Right";
  xk_Prior, "Prior";
  xk_Next, "Next";
  xk_BackSpace, "BackSpace";
  xk_Tab, "Tab";
  xk_Return, "Return";
]
let name_to_keysym = List.map (fun (a,b) -> b, a) keysym_to_name
