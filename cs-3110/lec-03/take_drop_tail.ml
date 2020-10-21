(* returns:  [from i j l] is the list containing the integers from
 *   [i] to [j], inclusive, followed by the list [l].
 * example:  [from 1 3 [0] = [1;2;3;0]] *)
let rec from i j l = if i > j then l else from i (j - 1) (j :: l)

(* returns:  [i -- j] is the list containing the integers from
 *   [i] to [j], inclusive.
 *)
let ( -- ) i j = from i j []
let longlist = 0 -- 1_000_000

let take' n lst =
  let rec loop i r = function
    | [] -> r
    | x :: xs -> if i >= n then r else loop (i + 1) (x :: r) xs
  in
  loop 0 [] lst |> List.rev
;;

let () = assert (take' 3 [ 1; 2; 3; 4; 5 ] = [ 1; 2; 3 ])
let () = assert (take' 3 [ 1; 2 ] = [ 1; 2 ])
let () = assert (take' 3 [] = [])
let () = assert (take' 0 [ 1; 2 ] = [])
let () = assert (take' 900_000 (0 -- 1_000_000) = 0 -- 899_999)
let () = assert (take' 9_000_000 (0 -- 1_000_000) = 0 -- 1_000_000)
let () = assert (take' 0 (0 -- 1_000_000) = [])

let drop' n lst =
  let rec loop i = function
    | [] -> []
    | r -> if i >= n then r else loop (i + 1) List.(tl r)
  in
  loop 0 lst
;;

let () = assert (drop' 3 [ 1; 2; 3; 4; 5 ] = [ 4; 5 ])
let () = assert (drop' 3 [ 1; 2 ] = [])
let () = assert (drop' 3 [] = [])
let () = assert (drop' 0 [ 1; 2 ] = [ 1; 2 ])
let () = assert (drop' 900_000 (0 -- 1_000_000) = 900_000 -- 1_000_000)
let () = assert (drop' 9_000_000 (0 -- 1_000_000) = [])
let () = assert (drop' 0 (0 -- 1_000_000) = 0 -- 1_000_000)
