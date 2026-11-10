module P = Tiny_printf.Printf

let error msg i = P.sprintf "%s at %d" msg i
let expected c = P.sprintf "expected %C" c
let encode c = P.sprintf "%%%02X" (Char.code c)
let unicode c = P.sprintf "\\u%04x" (Char.code c)
let number p f = P.sprintf "%.*g" p f
let format k a b = P.ksprintf k "%d of %d" a b
