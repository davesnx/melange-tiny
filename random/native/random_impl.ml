external random_seed : unit -> int array = "caml_sys_random_seed"

let seed () = Array.fold_left (fun a x -> (a * 31) lxor x) 0 (random_seed ())
