let product lst =
  let rec loop r = function
    | [] -> r
    | x :: xs -> loop (r * x) xs
  in
  loop 1 lst
;;

let () = assert (product [ 1; 2 ] = 2)
let () = assert (product [ 1; 2; 3 ] = 6)
let () = assert (product [ 3; 3; 2 ] = 18)
