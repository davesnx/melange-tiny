open Tiny_printf.Printf

let error msg i = sprintf (s ^^ lit " at " ^^ d) msg i
let expected c = sprintf (lit "expected " ^^ conv char_literal) c
let encode c = "%" ^ hex ~upper:true ~width:2 (Char.code c)
let unicode c = "\\u" ^ hex ~width:4 (Char.code c)
let number p f = float_g p f
let format k a b = ksprintf k (d ^^ lit " of " ^^ d) a b
