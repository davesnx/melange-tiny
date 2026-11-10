external chr : int -> string = "fromCharCode" [@@mel.scope "String"]

let md5_k =
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

let md5_r = [| 7; 12; 17; 22; 5; 9; 14; 20; 4; 11; 16; 23; 6; 10; 15; 21 |]

let md5 s =
  let get = Array.unsafe_get in
  let len = String.length s in
  let n = (((len + 8) lsr 6) + 1) lsl 6 in
  let byte i =
    if i < len then Char.code (String.unsafe_get s i)
    else if i = len then 0x80
    else if i >= n - 8 && i < n - 4 then ((len lsl 3) lsr (8 * (i - n + 8))) land 255
    else 0
  in
  let word i =
    byte i lor (byte (i + 1) lsl 8) lor (byte (i + 2) lsl 16) lor (byte (i + 3) lsl 24)
  in
  let h = [| 0x67452301; 0xefcdab89; 0x98badcfe; 0x10325476 |] in
  let block = ref 0 in
  while !block < n do
    let a = ref (get h 0) and b = ref (get h 1) and c = ref (get h 2) and d = ref (get h 3) in
    for i = 0 to 63 do
      let b' = !b and c' = !c and d' = !d in
      let round = i lsr 4 in
      let f =
        match round with
        | 0 -> (b' land c') lor ((b' lxor -1) land d')
        | 1 -> (d' land b') lor ((d' lxor -1) land c')
        | 2 -> b' lxor c' lxor d'
        | _ -> c' lxor (b' lor (d' lxor -1))
      in
      let g =
        match round with
        | 0 -> i
        | 1 -> ((5 * i) + 1) land 15
        | 2 -> ((3 * i) + 5) land 15
        | _ -> (7 * i) land 15
      in
      let x = !a + f + get md5_k i + word (!block + (4 * g)) in
      let s = get md5_r ((4 * round) + (i land 3)) in
      a := d';
      d := c';
      c := b';
      b := b' + ((x lsl s) lor (x lsr (32 - s)))
    done;
    Array.unsafe_set h 0 (get h 0 + !a);
    Array.unsafe_set h 1 (get h 1 + !b);
    Array.unsafe_set h 2 (get h 2 + !c);
    Array.unsafe_set h 3 (get h 3 + !d);
    block := !block + 64
  done;
  let out = ref "" in
  for i = 0 to 15 do
    out := !out ^ chr ((get h (i lsr 2) lsr (8 * (i land 3))) land 255)
  done;
  !out

let random_seed : unit -> int array =
 fun _ ->
  let seed : int = [%mel.raw "Math.floor(Math.random()*0x7fffffff)"] in
  [| seed |]

type state = { st : int array; mutable idx : int }

(* Melange compiles [Stdlib.Random.State.t] (_opam/lib/melange/melange/random.ml)
   from the same record declaration to the same JavaScript object. *)
external of_state : state -> Stdlib.Random.State.t = "%identity"
external to_state : Stdlib.Random.State.t -> state = "%identity"

module State = struct
  type t = Stdlib.Random.State.t

  let new_state () = of_state { st = Array.make 55 0; idx = 0 }

  let assign st1 st2 =
    let st1 = to_state st1 and st2 = to_state st2 in
    for i = 0 to 54 do
      Array.unsafe_set st1.st i (Array.unsafe_get st2.st i)
    done;
    st1.idx <- st2.idx

  let full_init s seed =
    let s = to_state s in
    let combine accu x = md5 (accu ^ string_of_int x) in
    let extract d =
      let c i = Char.code (String.unsafe_get d i) in
      c 0 + (c 1 lsl 8) + (c 2 lsl 16) + (c 3 lsl 24)
    in
    let seed = if Array.length seed = 0 then [| 0 |] else seed in
    let l = Array.length seed in
    for i = 0 to 54 do
      Array.unsafe_set s.st i i
    done;
    let accu = ref "x" in
    for i = 0 to 54 + if l > 55 then l else 55 do
      let j = i mod 55 in
      accu := combine !accu (Array.unsafe_get seed (i mod l));
      Array.unsafe_set s.st j
        ((Array.unsafe_get s.st j lxor extract !accu) land 0x3FFFFFFF)
    done;
    s.idx <- 0

  let make seed =
    let result = new_state () in
    full_init result seed;
    result

  let make_self_init () = make (random_seed ())

  let copy s =
    let result = new_state () in
    assign result s;
    result

  let bits s =
    let s = to_state s in
    s.idx <- (s.idx + 1) mod 55;
    let curval = Array.unsafe_get s.st s.idx in
    let newval =
      Array.unsafe_get s.st ((s.idx + 24) mod 55)
      + (curval lxor ((curval lsr 25) land 0x1F))
    in
    let newval30 = newval land 0x3FFFFFFF in
    Array.unsafe_set s.st s.idx newval30;
    newval30

  let rec intaux s n =
    let r = bits s in
    let v = r mod n in
    if r - v > 0x3FFFFFFF - n + 1 then intaux s n else v

  let int s bound =
    if bound > 0x3FFFFFFF || bound <= 0 then invalid_arg "Random.int"
    else intaux s bound

  let rec int31aux s n =
    let b1 = bits s in
    let b2 = bits s in
    let r = ((b2 land 0x3FFFC000) lsl 1) lor (b1 lsr 15) in
    let v = r mod n in
    if r - v > 0x7FFFFFFF - n + 1 then int31aux s n else v

  let full_int s bound =
    if bound <= 0 then invalid_arg "Random.full_int"
    else if bound > 0x3FFFFFFF then int31aux s bound
    else intaux s bound

  let rec int32aux s n =
    let b1 = Int32.of_int (bits s) in
    let b2 = Int32.shift_left (Int32.of_int (bits s land 1)) 30 in
    let r = Int32.logor b1 b2 in
    let v = Int32.rem r n in
    if Int32.sub r v > Int32.add (Int32.sub 0x7FFFFFFFl n) 1l then int32aux s n
    else v

  let int32 s bound =
    if bound <= 0l then invalid_arg "Random.int32" else int32aux s bound

  let rec int64aux s n =
    let b1 = Int64.of_int (bits s) in
    let b2 = Int64.shift_left (Int64.of_int (bits s)) 30 in
    let b3 = Int64.shift_left (Int64.of_int (bits s land 7)) 60 in
    let r = Int64.logor b1 (Int64.logor b2 b3) in
    let v = Int64.rem r n in
    if Int64.sub r v > Int64.add (Int64.sub 0x7FFFFFFFFFFFFFFFL n) 1L then
      int64aux s n
    else v

  let int64 s bound =
    if bound <= 0L then invalid_arg "Random.int64" else int64aux s bound

  let rawfloat s =
    let scale = 1073741824.0
    and r1 = Stdlib.float (bits s)
    and r2 = Stdlib.float (bits s) in
    ((r1 /. scale) +. r2) /. scale

  let float s bound = rawfloat s *. bound
  let bool s = bits s land 1 = 0

  let bits32 s =
    let b1 = Int32.(shift_right_logical (of_int (bits s)) 14) in
    let b2 = Int32.(shift_right_logical (of_int (bits s)) 14) in
    Int32.(logor b1 (shift_left b2 16))

  let bits64 s =
    let b1 = Int64.(shift_right_logical (of_int (bits s)) 9) in
    let b2 = Int64.(shift_right_logical (of_int (bits s)) 9) in
    let b3 = Int64.(shift_right_logical (of_int (bits s)) 8) in
    Int64.(logor b1 (logor (shift_left b2 21) (shift_left b3 42)))
end

let default =
  of_state {
    st =
      [| 0x3ae2522b; 0x1d8d4634; 0x15b4fad0; 0x18b14ace; 0x12f8a3c4; 0x3b086c47;
         0x16d467d6; 0x101d91c7; 0x321df177; 0x0176c193; 0x1ff72bf1; 0x1e889109;
         0x0b464b18; 0x2b86b97c; 0x0891da48; 0x03137463; 0x085ac5a1; 0x15d61f2f;
         0x3bced359; 0x29c1c132; 0x3a86766e; 0x366d8c86; 0x1f5b6222; 0x3ce1b59f;
         0x2ebf78e1; 0x27cd1b86; 0x258f3dc3; 0x389a8194; 0x02e4c44c; 0x18c43f7d;
         0x0f6e534f; 0x1e7df359; 0x055d0b7e; 0x10e84e7e; 0x126198e4; 0x0e7722cb;
         0x1cbede28; 0x3391b964; 0x3d40e92a; 0x0c59933d; 0x0b8cd0b7; 0x24efff1c;
         0x2803fdaa; 0x08ebc72e; 0x0f522e32; 0x05398edc; 0x2144a04c; 0x0aef3cbd;
         0x01ad4719; 0x35b93cd6; 0x2a559d4f; 0x1e6fd768; 0x26e27f36; 0x186f18c3;
         0x2fbf967a |];
    idx = 0;
  }

let bits () = State.bits default
let int bound = State.int default bound
let full_int bound = State.full_int default bound
let int32 bound = State.int32 default bound
let int64 bound = State.int64 default bound
let float scale = State.float default scale
let bool () = State.bool default
let bits32 () = State.bits32 default
let bits64 () = State.bits64 default
let full_init seed = State.full_init default seed
let init seed = State.full_init default [| seed |]
let self_init () = full_init (random_seed ())
let get_state () = State.copy default
let set_state s = State.assign default s
