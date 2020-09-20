let ( +/. ) a b = (a +. b) /. 2.
let () = assert (1.0 +/. 2.0 = 1.5)
let () = assert (0. +/. 0. = 0.)
