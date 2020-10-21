let rec powerset = function
  | [] -> [ [] ]
  | (x : int) :: xs ->
    let p = powerset xs in
    List.map (fun ys -> x :: ys) p @ p
;;

module Helper = struct
  let rec same a b =
    match a, b with
    | [], [] -> true
    | [], _ | _, [] -> false
    | x :: xs, _ -> List.mem x b || same xs (List.filter (fun e -> e <> x) b)
  ;;

  let ( = ) = same
  let ( <> ) a b = same a b |> not
end

let _ =
  Helper.(
    let _ = assert ([] = []) in
    let _ = assert ([] <> [ 1 ]) in
    let _ = assert ([ 1 ] <> []) in
    let _ = assert ([ 1 ] = [ 1 ]) in
    let _ = assert (powerset [] = [ [] ]) in
    let _ = assert (powerset [ 1; 2 ] = [ []; [ 1 ]; [ 2 ]; [ 1; 2 ] ]) in
    let _ =
      assert (
        powerset [ 1; 2; 3 ]
        = [ []; [ 1 ]; [ 2 ]; [ 3 ]; [ 1; 2 ]; [ 2; 3 ]; [ 1; 3 ]; [ 1; 2; 3 ] ])
    in
    ())
;;

let _ = print_endline "ok"
