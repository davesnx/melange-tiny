type t

external big : 'a -> t = "BigInt"
external number : t -> float = "Number"
external string : t -> string = "String"
external ( +! ) : t -> t -> t = "%addfloat"
external ( -! ) : t -> t -> t = "%subfloat"
external ( *! ) : t -> t -> t = "%mulfloat"
external ( /! ) : t -> t -> t = "%divfloat"
external ( %! ) : t -> t -> t = "caml_fmod_float" "fmod"
external negate : t -> t = "%negfloat"
external ( &! ) : t -> t -> t = "%andint"
external ( |! ) : t -> t -> t = "%orint"
external ( ^! ) : t -> t -> t = "%xorint"
external ( <<! ) : t -> t -> t = "%lslint"
external ( >>! ) : t -> t -> t = "%asrint"

let lt (a : t) (b : t) = (Obj.magic a : float) < (Obj.magic b : float)
let eq (a : t) (b : t) = (Obj.magic a : float) = (Obj.magic b : float)
let zero = big 0
let one = big 1
let minus_one = big (-1)
let two32 = big "4294967296"
let two64 = big "18446744073709551616"
let unsigned m x = x &! (m -! one)

let signed m x =
  let r = unsigned m x in
  if lt r (m /! big 2) then r else r -! m

let wrap x = signed two64 x
let max_int = big "9223372036854775807"
let min_int = big "-9223372036854775808"
let neg x = wrap (negate x)
let add a b = wrap (a +! b)
let sub a b = wrap (a -! b)
let mul a b = wrap (a *! b)
let div a b = if eq b zero then raise Division_by_zero else wrap (a /! b)
let rem a b = if eq b zero then raise Division_by_zero else a %! b
let succ x = add x one
let pred x = sub x one
let abs x = if lt x zero then neg x else x
let logand a b = a &! b
let logor a b = a |! b
let logxor a b = a ^! b
let lognot x = x ^! minus_one
let shift_left x n = wrap (x <<! big (n land 63))
let shift_right x n = x >>! big (n land 63)
let shift_right_logical x n = wrap (unsigned two64 x >>! big (n land 63))
let of_int (n : int) = big n
let to_int x : int = Obj.magic (number (signed two32 x))

let of_float f =
  if f -. f = 0. then wrap (big (f -. mod_float f 1.)) else zero

let to_float = number
let to_string = string
let compare a b = if lt a b then -1 else if lt b a then 1 else 0
let equal = eq
let min a b = if lt b a then b else a
let max a b = if lt a b then b else a

let of_string s =
  let fail () = failwith "Int64.of_string" in
  let n = String.length s in
  let i, negative =
    if n > 0 && s.[0] = '-' then (1, true)
    else if n > 0 && s.[0] = '+' then (1, false)
    else (0, false)
  in
  let i, base, signed =
    if i + 1 < n && s.[i] = '0' then
      match s.[i + 1] with
      | 'x' | 'X' -> (i + 2, 16, false)
      | 'o' | 'O' -> (i + 2, 8, false)
      | 'b' | 'B' -> (i + 2, 2, false)
      | 'u' | 'U' -> (i + 2, 10, false)
      | _ -> (i, 10, true)
    else (i, 10, true)
  in
  let digit c =
    match c with
    | '0' .. '9' -> Char.code c - 48
    | 'a' .. 'f' -> Char.code c - 87
    | 'A' .. 'F' -> Char.code c - 55
    | _ -> 99
  in
  if i >= n || digit s.[i] >= base then fail ();
  let b = big base in
  let v = ref zero in
  for j = i to n - 1 do
    match s.[j] with
    | '_' -> ()
    | c ->
        let d = digit c in
        if d >= base then fail ();
        v := (!v *! b) +! big d
  done;
  let limit =
    if not signed then two64 -! one
    else if negative then big "9223372036854775808"
    else max_int
  in
  if lt limit !v then fail ();
  wrap (if negative then negate !v else !v)

let of_string_opt s = try Some (of_string s) with Failure _ -> None
