let m = 0xFFFF_FFFF

type t = { mutable x : int; mutable y : int; mutable z : int; mutable w : int }

let s = { x = 0; y = 0; z = 0; w = 1 }

let next () =
  let t = s.x lxor ((s.x lsl 11) land m) in
  s.x <- s.y;
  s.y <- s.z;
  s.z <- s.w;
  s.w <- s.w lxor (s.w lsr 19) lxor t lxor (t lsr 8);
  s.w

let mix a =
  let a = a lxor ((a lsl 13) land m) in
  let a = a lxor (a lsr 17) in
  a lxor ((a lsl 5) land m)

let init seed =
  let a = (seed lxor ((seed asr 16) asr 16) lxor 0x6C078965) land m in
  s.x <- mix a;
  s.y <- mix s.x;
  s.z <- mix s.y;
  s.w <- mix s.z;
  if s.x lor s.y lor s.z lor s.w = 0 then s.w <- 1;
  for _ = 1 to 8 do
    ignore (next ())
  done

let () = init 0
let self_init () = init (Random_impl.seed ())
let bits () = next () lsr 2

let rec intaux n =
  let r = bits () in
  let v = r mod n in
  if r - v > 0x3FFFFFFF - n + 1 then intaux n else v

let int bound =
  if bound > 0x3FFFFFFF || bound <= 0 then invalid_arg "Random.int"
  else intaux bound

let float bound =
  let hi = bits () in
  let lo = bits () lsr 7 in
  (Stdlib.float hi *. 8388608. +. Stdlib.float lo) /. 9007199254740992. *. bound

let bool () = bits () land 1 = 0
