let rec fib = function
  | 1 -> 1
  | 2 -> 1
  | n -> fib (n - 1) + fib (n - 2)
;;

let rec fib' n = if n > 2 then fib' (n - 1) + fib' (n - 2) else 1

let fac i =
  let rec loop n i' = if i' < 1 then n else loop (i' * n) (i' - 1) in
  loop 1 i
;;

let fib'' n =
  let rec loop sum last i = if i < n then loop (sum + last) sum (i + 1) else sum in
  loop 0 1 0
;;

let check fn =
  let () = assert (fn 1 = 1) in
  let () = assert (fn 2 = 1) in
  let () = assert (fn 3 = 2) in
  let () = assert (fn 4 = 3) in
  let () = assert (fn 5 = 5) in
  let () = assert (fn 6 = 8) in
  ()
;;

let () = check fib
let () = check fib'
let () = check fib''
