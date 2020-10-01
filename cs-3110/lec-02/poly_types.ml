(* val f = bool -> bool *)
let f x = if x then x else x

(* val g = 'a -> bool -> 'a *)
let g x y = if y then x else x

(* val h = bool -> 'a  -> 'a -> 'a *)
let h x y z = if x then y else z

(* val i = bool -> 'a -> 'b -> 'a  *)
let i x y _ = if x then y else y
