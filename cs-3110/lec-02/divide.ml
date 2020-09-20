let divide n d = n /. d
let () = assert (divide 1. 2. = 0.5)
let () = assert (divide 1. 0. = infinity)
