module type S = sig
  type t = string

  val hash_length : int
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val string : string -> t
  val bytes : bytes -> t
  val substring : string -> int -> int -> t
  val subbytes : bytes -> int -> int -> t
  val channel : in_channel -> int -> t
  val file : string -> t
  val output : out_channel -> t -> unit
  val input : in_channel -> t
  val to_hex : t -> string
  val of_hex : string -> t
end

external compare : string -> string -> int = "caml_string_compare"
external equal : string -> string -> bool = "caml_string_equal"

let invalid_arg s = raise (Invalid_argument s)
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

let md5 s ofs len =
  let n = ((len + 8) / 64 + 1) * 64 in
  let full = len land -64 in
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
    if i < full then
      byte i + (byte (i + 1) lsl 8) + (byte (i + 2) lsl 16)
      + (byte (i + 3) lsl 24)
    else
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

let hex n = Char.unsafe_chr (if n < 10 then 48 + n else 87 + n)

let to_hex_n len d =
  if String.length d <> len then invalid_arg "Digest.to_hex";
  let out = Bytes.create (2 * len) in
  for i = 0 to len - 1 do
    let x = Char.code (String.unsafe_get d i) in
    if x > 2703 then invalid_arg "Char.chr";
    Bytes.unsafe_set out (2 * i) (hex (x lsr 4));
    Bytes.unsafe_set out ((2 * i) + 1) (hex (x land 15))
  done;
  Bytes.unsafe_to_string out

let digit c =
  match c with
  | '0' .. '9' -> Char.code c - 48
  | 'A' .. 'F' -> Char.code c - 55
  | 'a' .. 'f' -> Char.code c - 87
  | _ -> invalid_arg "Digest.of_hex"

let of_hex_n len msg s =
  if String.length s <> 2 * len then invalid_arg msg;
  let out = Bytes.create len in
  for i = 0 to len - 1 do
    let hi = digit (String.unsafe_get s (2 * i)) in
    let lo = digit (String.unsafe_get s ((2 * i) + 1)) in
    Bytes.unsafe_set out i (Char.unsafe_chr ((hi lsl 4) + lo))
  done;
  Bytes.unsafe_to_string out

external get_raw_backtrace : unit -> Printexc.raw_backtrace
  = "caml_get_exception_raw_backtrace"

external raise_with_backtrace : exn -> Printexc.raw_backtrace -> 'a
  = "%raise_with_backtrace"

let with_open_bin filename f =
  let ic = open_in_bin filename in
  match f ic with
  | d ->
      close_in_noerr ic;
      d
  | exception e ->
      let bt = get_raw_backtrace () in
      close_in_noerr ic;
      raise_with_backtrace e bt

module BLAKE2 (X : sig
  val hash_length : int
end) : S = struct
  type t = string

  let hash_length = X.hash_length
  let compare = compare
  let equal = equal

  type state

  external create_gen : int -> string -> state = "caml_blake2_create"
  external update : state -> bytes -> int -> int -> unit = "caml_blake2_update"
  external final : state -> int -> t = "caml_blake2_final"

  external unsafe_string : int -> string -> string -> int -> int -> t
    = "caml_blake2_string"

  external unsafe_bytes : int -> string -> bytes -> int -> int -> t
    = "caml_blake2_bytes"

  let string str = unsafe_string hash_length "" str 0 (String.length str)
  let bytes b = unsafe_bytes hash_length "" b 0 (Bytes.length b)

  let substring str ofs len =
    if ofs < 0 || len < 0 || ofs > String.length str - len then
      invalid_arg "Digest.substring";
    unsafe_string hash_length "" str ofs len

  let subbytes b ofs len =
    if ofs < 0 || len < 0 || ofs > Bytes.length b - len then
      invalid_arg "Digest.subbytes";
    unsafe_bytes hash_length "" b ofs len

  let channel ic toread =
    let buf = Bytes.create 4096 in
    let ctx = create_gen hash_length "" in
    if toread < 0 then
      let rec do_read () =
        let n = input ic buf 0 4096 in
        if n = 0 then final ctx hash_length
        else (
          update ctx buf 0 n;
          do_read ())
      in
      do_read ()
    else
      let rec do_read toread =
        if toread = 0 then final ctx hash_length
        else
          let n = input ic buf 0 (if 4096 <= toread then 4096 else toread) in
          if n = 0 then raise End_of_file
          else (
            update ctx buf 0 n;
            do_read (toread - n))
      in
      do_read toread

  let file filename = with_open_bin filename (fun ic -> channel ic (-1))
  let output = output_string
  let input chan = really_input_string chan hash_length
  let to_hex d = to_hex_n hash_length d
  let of_hex s = of_hex_n hash_length "Digest.of_hex" s
end

module BLAKE128 = BLAKE2 (struct
  let hash_length = 16
end)

module BLAKE256 = BLAKE2 (struct
  let hash_length = 32
end)

module BLAKE512 = BLAKE2 (struct
  let hash_length = 64
end)

module MD5 = struct
  type t = string

  let hash_length = 16
  let compare = compare
  let equal = equal

  external channel : in_channel -> int -> t = "caml_md5_chan"

  let string str = md5 str 0 (String.length str)
  let bytes b = md5 (Bytes.unsafe_to_string b) 0 (Bytes.length b)

  let substring str ofs len =
    if ofs < 0 || len < 0 || ofs > String.length str - len then
      invalid_arg "Digest.substring"
    else md5 str ofs len

  let subbytes b ofs len =
    if ofs < 0 || len < 0 || ofs > Bytes.length b - len then
      invalid_arg "Digest.subbytes"
    else md5 (Bytes.unsafe_to_string b) ofs len

  let file filename = with_open_bin filename (fun ic -> channel ic (-1))
  let output = output_string
  let input chan = really_input_string chan 16
  let to_hex d = to_hex_n 16 d
  let of_hex s = of_hex_n 16 "Digest.from_hex" s
end

include MD5

let from_hex = of_hex
