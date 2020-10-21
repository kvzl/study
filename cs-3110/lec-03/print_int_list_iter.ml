let print_int_list' = List.iter (fun x -> string_of_int x |> print_endline)
let () = print_int_list' [ 1; 2; 3 ]
