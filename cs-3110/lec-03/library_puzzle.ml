let last lst =
  let n = List.length lst in
  List.nth lst (n - 1)
;;

let () = assert (last [ 1; 2; 3 ] = 3)
let () = assert (last [ 3; 2; 1 ] = 1)
let any_zeroes = List.exists (fun n -> n = 0)
let any_zeroes' = List.mem 0

let check fn =
  let () = assert (fn [ 0; 0; 0 ] = true) in
  let () = assert (fn [ 0; 2; 4 ] = true) in
  let () = assert (fn [ 2; 4 ] = false) in
  let () = assert (fn [] = false) in
  ()
;;

let () = check any_zeroes
let () = check any_zeroes'
