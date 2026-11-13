open Tiny_printf
open Tiny_hashtbl
open Tiny_random
open Tiny_md5
open Tiny_buffer

let message msg i = Printf.sprintf "%s at %d" msg i

let run () =
  let t = Hashtbl.create 1 in
  Hashtbl.replace t "k" (Printf.sprintf "%.3g" 3.14159);
  Random.init 1;
  let n = string_of_bool (Random.int 10 < 10) in
  let b = Buffer.create 16 in
  Buffer.add_string b (message (Hashtbl.find t "k" ^ " " ^ n ^ " " ^ Digest.(to_hex (string "a"))) 42);
  Buffer.add_char b '\n';
  Buffer.add_substitute b (fun v -> string_of_int (String.length v)) "buffer $name ${x}";
  Buffer.contents b
