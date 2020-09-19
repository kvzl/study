let validate_date d m =
  let is_knuckle m' = List.mem m' [ "Jan"; "Mar"; "May"; "Jul"; "Aug"; "Oct"; "Dec" ]
  and is_valley m' = List.mem m' [ "Apr"; "Jun"; "Sep"; "Nov" ] in
  let last_day =
    if is_knuckle m then Some 31 else if is_valley m then Some 30 else None
  in
  d >= 1
  &&
  match last_day with
  | Some n -> d <= n
  | None -> m = "Feb" && d <= 28
;;

let validate_date' d m =
  let last_day =
    if List.mem m [ "Jan"; "Mar"; "May"; "Jul"; "Aug"; "Oct"; "Dec" ]
    then Some 31
    else if List.mem m [ "Apr"; "Jun"; "Sep"; "Nov" ]
    then Some 30
    else None
  in
  d >= 1
  &&
  match last_day with
  | Some n -> d <= n
  | None -> m = "Feb" && d <= 28
;;

let validate_date'' d m =
  let months =
    [ "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" ]
  in
  let rec is_exceeded i = function
    | [] -> false
    | x :: xs ->
      if x <> m
      then is_exceeded (i + 1) xs
      else (
        let i' = i mod 7 in
        if i' = 2
        then d <= 28
        else if i' mod 2 = 1
        then d <= 31
        else if i' mod 2 = 0
        then d <= 30
        else false)
  in
  d >= 1 && is_exceeded 1 months
;;

let check fn =
  let () = assert (fn (-1) "Invalid" = false) in
  let () = assert (fn (-1) "Nov" = false) in
  let () = assert (fn 30 "Nov" = true) in
  let () = assert (fn 31 "Nov" = false) in
  let () = assert (fn (-1) "Dec" = false) in
  let () = assert (fn 30 "Dec" = true) in
  let () = assert (fn 31 "Dec" = true) in
  let () = assert (fn (-1) "Feb" = false) in
  let () = assert (fn 28 "Feb" = true) in
  let () = assert (fn 30 "Feb" = false) in
  ()
;;

let () = check validate_date
let () = check validate_date'
let () = check validate_date''
