open Tiny_printf
open Tiny_hashtbl
open Tiny_random
open Tiny_md5

let message msg i = Printf.sprintf Printf.(s ^^ lit " at " ^^ d) msg i

let run () =
  let t = Hashtbl.create 1 in
  Hashtbl.replace t "k" (Printf.float_g 3 3.14159);
  Random.init 1;
  let n = string_of_int (Random.int 10 + 1) in
  message (Hashtbl.find t "k" ^ " " ^ n ^ " " ^ Digest.(to_hex (string "a"))) 42
