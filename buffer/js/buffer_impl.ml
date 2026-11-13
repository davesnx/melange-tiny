type t = string ref

external chars : int array -> string = "fromCharCode"
[@@mel.scope "String"] [@@mel.variadic]

external slice : string -> int -> int -> string = "slice" [@@mel.send]
external slice_bytes : bytes -> int -> int -> bytes = "slice" [@@mel.send]
external code : Uchar.t -> int = "%identity"

let invalid_arg s = raise (Invalid_argument s)

let create _ = ref ""
let contents b = !b
let length b = String.length !b
let clear b = b := ""
let reset = clear
let to_bytes b = Bytes.unsafe_of_string !b
let add_string b s = b := !b ^ s
let add_char b c = add_string b (chars [| Char.code c |])
let add_int8 b x = add_string b (chars [| x |])
let add_uint8 = add_int8
let add_buffer b c = add_string b !c

let sub b ofs len =
  if ofs < 0 || len < 0 || ofs > length b - len then invalid_arg "Buffer.sub";
  slice !b ofs (ofs + len)

let blit src srcoff dst dstoff len =
  if len < 0 || srcoff < 0 || srcoff > length src - len || dstoff < 0
     || dstoff > Bytes.length dst - len
  then invalid_arg "Buffer.blit";
  Bytes.unsafe_blit_string !src srcoff dst dstoff len

let nth b ofs =
  if ofs < 0 || ofs >= length b then invalid_arg "Buffer.nth";
  String.unsafe_get !b ofs

let truncate b len =
  if len < 0 || len > length b then invalid_arg "Buffer.truncate";
  b := slice !b 0 len

let add_substring b s ofs len =
  if ofs < 0 || len < 0 || ofs > String.length s - len then
    invalid_arg "Buffer.add_substring";
  add_string b (slice s ofs (ofs + len))

let add_subbytes b s ofs len =
  if ofs < 0 || len < 0 || ofs > Bytes.length s - len then
    invalid_arg "Buffer.add_subbytes";
  add_string b (Bytes.unsafe_to_string (slice_bytes s ofs (ofs + len)))

let add_bytes b s = add_subbytes b s 0 (Bytes.length s)

let add_utf_8_uchar b u =
  let u = code u in
  let c s = 0x80 lor ((u lsr s) land 0x3f) in
  add_string b
    (if u < 0x80 then chars [| u |]
     else if u < 0x800 then chars [| 0xc0 lor (u lsr 6); c 0 |]
     else if u < 0x10000 then chars [| 0xe0 lor (u lsr 12); c 6; c 0 |]
     else chars [| 0xf0 lor (u lsr 18); c 12; c 6; c 0 |])

let high u = 0xd800 lor ((u - 0x10000) lsr 10)
let low u = 0xdc00 lor (u land 0x3ff)

let add_uint16_le b x = add_string b (chars [| x land 0xff; (x lsr 8) land 0xff |])
let add_uint16_be b x = add_string b (chars [| (x lsr 8) land 0xff; x land 0xff |])
let add_uint16_ne = add_uint16_le
let add_int16_ne = add_uint16_le
let add_int16_le = add_uint16_le
let add_int16_be = add_uint16_be

let add_utf_16le_uchar b u =
  let u = code u in
  if u < 0x10000 then add_uint16_le b u
  else (
    add_uint16_le b (high u);
    add_uint16_le b (low u))

let add_utf_16be_uchar b u =
  let u = code u in
  if u < 0x10000 then add_uint16_be b u
  else (
    add_uint16_be b (high u);
    add_uint16_be b (low u))

let add_int32_le b x =
  let x = Int32.to_int x in
  add_uint16_le b x;
  add_uint16_le b (x lsr 16)

let add_int32_be b x =
  let x = Int32.to_int x in
  add_uint16_be b (x lsr 16);
  add_uint16_be b x

let add_int32_ne = add_int32_le
let top x = Int64.to_int32 (Int64.shift_right_logical x 32)

let add_int64_le b x =
  add_int32_le b (Int64.to_int32 x);
  add_int32_le b (top x)

let add_int64_be b x =
  add_int32_be b (top x);
  add_int32_be b (Int64.to_int32 x)

let add_int64_ne = add_int64_le

let to_seq b =
  let rec next i () =
    if i >= length b then Seq.Nil else Seq.Cons (String.unsafe_get !b i, next (i + 1))
  in
  next 0

let to_seqi b =
  let rec next i () =
    if i >= length b then Seq.Nil
    else Seq.Cons ((i, String.unsafe_get !b i), next (i + 1))
  in
  next 0

let rec add_seq b s =
  match s () with
  | Seq.Nil -> ()
  | Seq.Cons (c, s) ->
      add_char b c;
      add_seq b s

let of_seq s =
  let b = create 0 in
  add_seq b s;
  b

let add_substitute b f s =
  let n = String.length s in
  let at i = String.unsafe_get s i in
  let rec name_end i =
    match if i < n then at i else ' ' with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> name_end (i + 1)
    | _ -> i
  in
  let rec close o c depth i =
    if i >= n then raise Not_found
    else if at i = o then close o c (depth + 1) (i + 1)
    else if at i <> c then close o c depth (i + 1)
    else if depth = 0 then i
    else close o c (depth - 1) (i + 1)
  in
  let name i =
    if i >= n then raise Not_found;
    let closing = match at i with '(' -> ')' | '{' -> '}' | _ -> ' ' in
    if closing <> ' ' then
      let j = close (at i) closing 0 (i + 1) in
      (slice s (i + 1) j, j + 1)
    else
      let j = name_end i in
      if j = i then raise Not_found;
      (slice s i j, j)
  in
  let rec go prev i =
    if i >= n then (if prev = '\\' then add_char b prev)
    else
      match (at i, prev) with
      | '$', '\\' ->
          add_char b '$';
          go ' ' (i + 1)
      | '$', _ -> (
          match name (i + 1) with
          | v, j ->
              add_string b (f v);
              go ' ' j
          | exception Not_found ->
              add_char b '$';
              go ' ' (i + 1))
      | c, _ ->
          if prev = '\\' then add_char b prev;
          if c <> '\\' then add_char b c;
          go c (i + 1)
  in
  go ' ' 0

let output_buffer oc b = output_bytes oc (to_bytes b)

let add_channel b ic len =
  if len < 0 then invalid_arg "Buffer.add_channel";
  let s = Bytes.create len in
  let rec read n =
    match if n < len then input ic s n (len - n) else 0 with
    | 0 -> n
    | r -> read (n + r)
  in
  let n = read 0 in
  add_subbytes b s 0 n;
  if n < len then raise End_of_file
