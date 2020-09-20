let add x y = x + y

(* produces a integer: *)
let n = add 5 1
let n = (add 5) 1

(* produces a function: *)
let fn = add 5

(* produces a error: *)
(* let _ = add (5 1) *)
