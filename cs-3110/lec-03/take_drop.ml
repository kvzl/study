let take n lst = if n > List.length lst then lst else List.filteri (fun i _ -> i < n) lst
let () = assert (take 3 [ 1; 2; 3; 4; 5 ] = [ 1; 2; 3 ])
let () = assert (take 3 [ 1; 2 ] = [ 1; 2 ])
let () = assert (take 3 [] = [])
let () = assert (take 0 [ 1; 2 ] = [])
let drop n lst = if n > List.length lst then [] else List.filteri (fun i _ -> i >= n) lst
let () = assert (drop 3 [ 1; 2; 3; 4; 5 ] = [ 4; 5 ])
let () = assert (drop 3 [ 1; 2 ] = [])
let () = assert (drop 3 [] = [])
let () = assert (drop 0 [ 1; 2 ] = [ 1; 2 ])
