
type keySym = int
type modifiers = int

let shiftMask = 1

let controlMask = 4
let mod1Mask = 8
let mod2Mask = 16
let mod3Mask = 32
let mod4Mask = 64
let mod5Mask = 128

(* XT because used to be xterm events *)
type event =
  XTKeyPress of modifiers * string * keySym
| XTButtonPress of modifiers * int * int * int
| XTMouseMotion of modifiers * int * int * int
| XTResize of int * int
