let fifth_or_zero lst =
  let len = List.length lst in
  if len <> 5 then 0 else List.nth lst 4
;;

let () = assert (fifth_or_zero [ 1; 2; 3; 4; 5 ] = 5)
let () = assert (fifth_or_zero [ 1; 2; 3; 4 ] = 0)
let () = assert (fifth_or_zero [ 1; 2; 3; 4; 0 ] = 0)
let () = assert (fifth_or_zero [] = 0)
let sorted lst = lst |> List.sort Stdlib.compare |> List.rev
let () = assert (sorted [] = [])
let () = assert (sorted [ 3; 2; 5; 1; 4 ] = [ 5; 4; 3; 2; 1 ])
