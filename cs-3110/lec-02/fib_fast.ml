let fib_fast n =
  let rec loop sum last i = if i < n then loop (sum + last) sum (i + 1) else sum in
  loop 0 1 0
;;

let fib_fast' n =
  let rec loop sum last i = if i < 1 then sum else loop (sum + last) sum (i - 1) in
  loop 0 1 n
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

let () = check fib_fast
let () = check fib_fast'
