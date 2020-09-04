let foo x = 
1

let bar = function
| 0 -> 1
| 1 -> 2
| _ -> 3

type t = 
| Bar of string
| Foo of string

type r = {
foo: string;
bar: int;
}

let foo x = 
match x with
| 0 -> 1
| 2 -> 
3

let foo x =
let x = 1 in
let x = 3 in
x + x

let foo x =
try
foo ()
with Not_found -> 1
