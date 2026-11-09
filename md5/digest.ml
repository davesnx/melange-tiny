type t = string

let compare = String.compare
let equal = String.equal
let mask = 0xFFFFFFFF

let k =
  [| 0xd76aa478; 0xe8c7b756; 0x242070db; 0xc1bdceee; 0xf57c0faf; 0x4787c62a;
     0xa8304613; 0xfd469501; 0x698098d8; 0x8b44f7af; 0xffff5bb1; 0x895cd7be;
     0x6b901122; 0xfd987193; 0xa679438e; 0x49b40821; 0xf61e2562; 0xc040b340;
     0x265e5a51; 0xe9b6c7aa; 0xd62f105d; 0x02441453; 0xd8a1e681; 0xe7d3fbc8;
     0x21e1cde6; 0xc33707d6; 0xf4d50d87; 0x455a14ed; 0xa9e3e905; 0xfcefa3f8;
     0x676f02d9; 0x8d2a4c8a; 0xfffa3942; 0x8771f681; 0x6d9d6122; 0xfde5380c;
     0xa4beea44; 0x4bdecfa9; 0xf6bb4b60; 0xbebfbc70; 0x289b7ec6; 0xeaa127fa;
     0xd4ef3085; 0x04881d05; 0xd9d4d039; 0xe6db99e5; 0x1fa27cf8; 0xc4ac5665;
     0xf4292244; 0x432aff97; 0xab9423a7; 0xfc93a039; 0x655b59c3; 0x8f0ccc92;
     0xffeff47d; 0x85845dd1; 0x6fa87e4f; 0xfe2ce6e0; 0xa3014314; 0x4e0811a1;
     0xf7537e82; 0xbd3af235; 0x2ad7d2bb; 0xeb86d391 |]

let r = [| 7; 12; 17; 22; 5; 9; 14; 20; 4; 11; 16; 23; 6; 10; 15; 21 |]

let unsafe_string s ofs len =
  let n = ((len + 8) / 64 + 1) * 64 in
  let byte i =
    if i < len then Char.code (String.unsafe_get s (ofs + i))
    else if i = len then 0x80
    else if i < n - 8 then 0
    else
      let j = i - n + 8 in
      if j < 4 then ((len lsl 3) lsr (8 * j)) land 255
      else ((len lsr 29) lsr (8 * (j - 4))) land 255
  in
  let word i =
    byte i lor (byte (i + 1) lsl 8) lor (byte (i + 2) lsl 16)
    lor (byte (i + 3) lsl 24)
  in
  let h = [| 0x67452301; 0xefcdab89; 0x98badcfe; 0x10325476 |] in
  let block = ref 0 in
  while !block < n do
    let get = Array.unsafe_get in
    let a = ref (get h 0) and b = ref (get h 1) and c = ref (get h 2) and d = ref (get h 3) in
    for i = 0 to 63 do
      let b' = !b and c' = !c and d' = !d in
      let round = i / 16 in
      let f =
        match round with
        | 0 -> (b' land c') lor (b' lxor -1 land d')
        | 1 -> (d' land b') lor (d' lxor -1 land c')
        | 2 -> b' lxor c' lxor d'
        | _ -> c' lxor (b' lor (d' lxor mask))
      in
      let g =
        match round with
        | 0 -> i
        | 1 -> ((5 * i) + 1) land 15
        | 2 -> ((3 * i) + 5) land 15
        | _ -> (7 * i) land 15
      in
      let x = (!a + f + get k i + word (!block + (4 * g))) land mask in
      let s = get r ((4 * round) + (i land 3)) in
      a := d';
      d := c';
      c := b';
      b := (b' + (((x lsl s) lor (x lsr (32 - s))) land mask)) land mask
    done;
    let add j v = Array.unsafe_set h j ((get h j + v) land mask) in
    add 0 !a;
    add 1 !b;
    add 2 !c;
    add 3 !d;
    block := !block + 64
  done;
  let out = Bytes.create 16 in
  for i = 0 to 15 do
    Bytes.unsafe_set out i
      (Char.unsafe_chr ((Array.unsafe_get h (i / 4) lsr (8 * (i land 3))) land 255))
  done;
  Bytes.unsafe_to_string out

let string s = unsafe_string s 0 (String.length s)

let substring s ofs len =
  if ofs < 0 || len < 0 || ofs > String.length s - len then
    invalid_arg "Digest.substring"
  else unsafe_string s ofs len

let to_hex d =
  if String.length d <> 16 then invalid_arg "Digest.to_hex";
  let out = Bytes.create 32 in
  for i = 0 to 31 do
    let x = Char.code (String.unsafe_get d (i / 2)) in
    let n = if i land 1 = 0 then x lsr 4 else x land 15 in
    Bytes.unsafe_set out i (Char.unsafe_chr (if n < 10 then 48 + n else 87 + n))
  done;
  Bytes.unsafe_to_string out

let digit c =
  match c with
  | '0' .. '9' -> Char.code c - 48
  | 'A' .. 'F' -> Char.code c - 55
  | 'a' .. 'f' -> Char.code c - 87
  | _ -> invalid_arg "Digest.of_hex"

let of_hex s =
  if String.length s <> 32 then invalid_arg "Digest.from_hex";
  let out = Bytes.create 16 in
  for i = 0 to 15 do
    let hi = digit (String.unsafe_get s (2 * i)) in
    let lo = digit (String.unsafe_get s ((2 * i) + 1)) in
    Bytes.unsafe_set out i (Char.unsafe_chr ((hi lsl 4) + lo))
  done;
  Bytes.unsafe_to_string out

let from_hex = of_hex
