let error msg i = Printf.sprintf "%s at %d" msg i
let expected c = Printf.sprintf "expected %C" c
let encode c = Printf.sprintf "%%%02X" (Char.code c)
let unicode c = Printf.sprintf "\\u%04x" (Char.code c)
let number p f = Printf.sprintf "%.*g" p f
let format k a b = Printf.ksprintf k "%d of %d" a b
