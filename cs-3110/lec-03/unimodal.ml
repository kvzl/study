let is_unimodal lst =
  let rec dec = function
    | [] -> true
    | [ _ ] -> true
    | x1 :: x2 :: xs -> if x1 >= x2 then dec (x2 :: xs) else false
  in
  let rec inc = function
    | [] -> true
    | [ _ ] -> true
    | x1 :: x2 :: xs -> if x1 <= x2 then inc (x2 :: xs) else dec xs
  in
  inc lst
;;

let () = assert (is_unimodal [ 1; 1; 1; 1; 1 ] = true)
let () = assert (is_unimodal [ 1; 2; 3; 3; 3 ] = true)
let () = assert (is_unimodal [ 1; 1; 3; 3; 4; 3 ] = true)
let () = assert (is_unimodal [ 5; 4; 2; 1 ] = true)
let () = assert (is_unimodal [] = true)
let () = assert (is_unimodal [ 1; 2; 3; 2; 3; 5 ] = false)
let () = assert (is_unimodal [ 5; 4; 2; 1; 3 ] = false)
