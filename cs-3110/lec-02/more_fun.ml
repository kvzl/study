let cube x = x * x * x
let () = assert (cube 2 = 8)
let () = assert (cube 3 = 27)
let () = assert (cube 4 = 64)
let sign x = if x > 0 then 1 else if x = 0 then 0 else -1
let () = assert (sign 1 = 1)
let () = assert (sign 2 = 1)
let () = assert (sign 0 = 0)
let () = assert (sign (-1) = -1)
let () = assert (sign (-3) = -1)

let circle r =
  let pi = 3.14 in
  pi *. r *. r
;;

let () = assert (circle 3. *. 100. = 2826.)
let () = assert (circle 5. *. 100. = 7850.)
