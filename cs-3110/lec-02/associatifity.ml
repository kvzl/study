let add x y = x + y

(* produces a integer: *)
let _ = add 5 1
let _ = (add 5) 1

(* produces a function: *)
let fn = add 5

(* produces a error: *)
(* let _ = add (5 1) *)
