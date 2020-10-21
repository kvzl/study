let is_first_bigred = function
  | [] -> false
  | x :: _ -> x = "bigred"
;;

let () = assert (is_first_bigred [ "bigred"; "2"; "3" ] = true)
let () = assert (is_first_bigred [ "1"; "bigred"; "2"; "3" ] = false)

let has_two_or_four = function
  | [ _; _ ] | [ _; _; _; _ ] -> true
  | _ -> false
;;

let () = assert (has_two_or_four [ 1; 2; 3; 4; 5 ] = false)
let () = assert (has_two_or_four [ 1; 2; 3; 4 ] = true)
let () = assert (has_two_or_four [ 1; 2; 3 ] = false)
let () = assert (has_two_or_four [ 1; 2 ] = true)
let () = assert (has_two_or_four [ 1 ] = false)
let () = assert (has_two_or_four [] = false)

let first_two_equal = function
  | x :: y :: _ -> x = y
  | _ -> false
;;

let () = assert (first_two_equal [ 1; 2; 3 ] = false)
let () = assert (first_two_equal [ 1 ] = false)
let () = assert (first_two_equal [] = false)
let () = assert (first_two_equal [ 1; 1 ] = true)
