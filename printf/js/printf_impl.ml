external to_exponential : float -> int -> string = "toExponential"
[@@mel.send]

external to_precision : float -> int -> string = "toPrecision" [@@mel.send]
external parse_int : string -> int = "parseInt"

let trim s =
  if Js.String.includes ~search:"." s then
    Js.String.replaceByRe ~regexp:[%re "/\\.?0+$/"] ~replacement:"" s
  else s

let float_g p x =
  if x <> x then "nan"
  else if x -. x <> 0. then if x > 0. then "inf" else "-inf"
  else
    let p = if p = 0 then 1 else if p > 100 then 100 else p in
    let s = to_exponential x (p - 1) in
    let i = Js.String.indexOf ~search:"e" s in
    let e = parse_int (Js.String.slice ~start:(i + 1) s) in
    if e < -4 || e >= p then
      let a = if e < 0 then -e else e in
      trim (Js.String.slice ~start:0 ~end_:i s)
      ^ (if e < 0 then "e-" else "e+")
      ^ (if a < 10 then "0" else "")
      ^ string_of_int a
    else trim (to_precision x p)
