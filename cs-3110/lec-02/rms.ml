let rms x y = (((x ** 2.) +. (y ** 2.)) /. 2.) ** 0.5

let rms_2 xs =
  List.(
    let len = length xs |> float_of_int in
    (xs |> map (fun x -> x ** 2.) |> fold_left ( +. ) 0.) /. len |> sqrt)
;;

let () = assert (rms 3. 4. = rms_2 [ 3.; 4. ])
let () = assert (rms 2. 3. = rms_2 [ 2.; 3. ])
