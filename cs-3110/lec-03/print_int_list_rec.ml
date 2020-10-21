let print_int_list lst =
  let rec aux = function
    | [] -> ()
    | x :: xs ->
      let () = string_of_int x |> print_endline in
      aux xs
  in
  aux lst
;;

let () = print_int_list [ 1; 2; 3 ]
