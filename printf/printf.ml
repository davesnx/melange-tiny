let int = string_of_int

let hex ?(upper = false) ?(width = 0) n =
  let digits = if upper then "0123456789ABCDEF" else "0123456789abcdef" in
  let rec go n acc =
    let acc = String.make 1 (String.unsafe_get digits (n land 15)) ^ acc in
    let n = n lsr 4 in
    if n = 0 then acc else go n acc
  in
  let s = go n "" in
  let pad = width - String.length s in
  if pad > 0 then String.make pad '0' ^ s else s

let float_g precision f =
  if precision < 0 then invalid_arg "Tiny_printf.Printf.float_g"
  else Printf_impl.float_g precision f

let char_literal c = "'" ^ Char.escaped c ^ "'"

type ('a, 'r) t = (string -> 'r) -> string -> 'a

let lit text k acc = k (acc ^ text)
let conv f k acc x = k (acc ^ f x)
let s k acc x = k (acc ^ x)
let d k acc x = k (acc ^ string_of_int x)
let ( ^^ ) f g k = f (g k)
let ksprintf k f = f k ""
let sprintf f = f (fun x -> x) ""
