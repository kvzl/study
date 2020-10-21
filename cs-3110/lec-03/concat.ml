let concat lst =
  let rec loop s = function
    | [] -> s
    | x :: xs -> loop (s ^ x) xs
  in
  loop "" lst
;;

let () = assert (concat [ "A"; "B" ] = "AB")
let () = assert (concat [ "A"; "B"; "" ] = "AB")
let () = assert (concat [ "" ] = "")
