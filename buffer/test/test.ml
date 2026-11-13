module T = Tiny_buffer.Buffer

module type B = sig
  type t

  include module type of Stdlib__Buffer with type t := t
end

module _ : B = T
module _ : module type of Tiny_buffer__Buffer with type t := Stdlib.Buffer.t = Stdlib.Buffer

let checks = ref 0
let failures = ref 0
let result f = match f () with v -> Ok v | exception e -> Error (Platform.describe e)

let show s =
  String.concat ""
    (List.map
       (fun c ->
         let n = Char.code c in
         if n >= 32 && n < 127 && n <> 92 then String.make 1 c else "\\" ^ string_of_int n ^ ";")
       (List.of_seq (String.to_seq s)))

let show_result = function Ok v -> show v | Error e -> "exception " ^ e

let report name ours expected =
  incr checks;
  if ours <> expected then (
    incr failures;
    Printf.printf "FAIL %s\n  tiny:   %s\n  stdlib: %s\n" name (show_result ours) (show_result expected))

let same name tiny stdlib = report name (result tiny) (result stdlib)
let code c = string_of_int (Char.code c)
let codes seq = String.concat "," (List.of_seq (Seq.map code seq))

let substitute v =
  match v with "nf" -> raise Not_found | "boom" -> failwith "boom" | v -> "<" ^ v ^ ">"

type op =
  | Create of int
  | Of_seq of string
  | Add_char of char
  | Add_string of string
  | Add_substring of string * int * int
  | Add_bytes of string
  | Add_subbytes of string * int * int
  | Add_utf_8 of int
  | Add_utf_16le of int
  | Add_utf_16be of int
  | Add_int of int * int
  | Add_int32 of int * int32
  | Add_int64 of int * int64
  | Add_seq of string
  | Add_self
  | Add_other of string
  | Add_substitute of string
  | Truncate of int
  | Clear
  | Reset
  | Nth of int
  | Sub of int * int
  | Blit of int * int * int * int
  | To_bytes
  | Seq_change of int

module Run (B : B) = struct
  let ints =
    [| B.add_int8; B.add_uint8; B.add_int16_ne; B.add_int16_le; B.add_int16_be;
       B.add_uint16_ne; B.add_uint16_le; B.add_uint16_be |]

  let int32s = [| B.add_int32_ne; B.add_int32_le; B.add_int32_be |]
  let int64s = [| B.add_int64_ne; B.add_int64_le; B.add_int64_be |]
  let bytes_codes b = codes (String.to_seq (Bytes.to_string b))

  let change b = function
    | 0 -> B.truncate b 0
    | 1 -> B.add_string b "xy"
    | 2 -> B.clear b; B.add_string b "pq"
    | 3 -> B.reset b
    | _ -> B.truncate b (B.length b / 2)

  let step b = function
    | Create _ | Of_seq _ -> ""
    | Add_char c -> B.add_char b c; ""
    | Add_string s -> B.add_string b s; ""
    | Add_substring (s, o, l) -> B.add_substring b s o l; ""
    | Add_bytes s -> B.add_bytes b (Bytes.of_string s); ""
    | Add_subbytes (s, o, l) -> B.add_subbytes b (Bytes.of_string s) o l; ""
    | Add_utf_8 u -> B.add_utf_8_uchar b (Uchar.of_int u); ""
    | Add_utf_16le u -> B.add_utf_16le_uchar b (Uchar.of_int u); ""
    | Add_utf_16be u -> B.add_utf_16be_uchar b (Uchar.of_int u); ""
    | Add_int (f, x) -> ints.(f) b x; ""
    | Add_int32 (f, x) -> int32s.(f) b x; ""
    | Add_int64 (f, x) -> int64s.(f) b x; ""
    | Add_seq s -> B.add_seq b (String.to_seq s); ""
    | Add_self -> B.add_buffer b b; ""
    | Add_other s ->
        let o = B.create 1 in
        B.add_string o s;
        B.add_buffer b o;
        B.add_char o 'z';
        ""
    | Add_substitute s -> B.add_substitute b substitute s; ""
    | Truncate n -> B.truncate b n; ""
    | Clear -> B.clear b; ""
    | Reset -> B.reset b; ""
    | Nth i -> code (B.nth b i)
    | Sub (o, l) -> B.sub b o l
    | Blit (o, n, d, l) ->
        let dst = Bytes.make n 'z' in
        B.blit b o dst d l;
        bytes_codes dst
    | To_bytes -> bytes_codes (B.to_bytes b)
    | Seq_change k -> (
        let s = B.to_seq b and si = B.to_seqi b in
        match (s (), si ()) with
        | Seq.Cons (c, s), Seq.Cons ((i, d), si) ->
            change b k;
            String.concat ";"
              [ code c; codes s; string_of_int i; code d;
                String.concat "," (List.of_seq (Seq.map (fun (i, c) -> string_of_int i ^ ":" ^ code c) si)) ]
        | _ -> "empty")

  let run ops =
    let b =
      match ops with
      | Create n :: _ -> B.create n
      | Of_seq s :: _ -> B.of_seq (String.to_seq s)
      | _ -> B.create 1
    in
    List.map
      (fun op ->
        let r = show_result (result (fun () -> step b op)) in
        String.concat " | "
          [ r; B.contents b; string_of_int (B.length b); codes (B.to_seq b);
            String.concat "," (List.of_seq (Seq.map (fun (i, c) -> string_of_int i ^ ":" ^ code c) (B.to_seqi b))) ])
      ops
end

module RT = Run (T)
module RS = Run (Stdlib.Buffer)

let compare_runs name ops =
  let rec go i ops a b =
    match (ops, a, b) with
    | op :: ops, x :: a, y :: b ->
        ignore op;
        report (Printf.sprintf "%s step %d" name i) (Ok x) (Ok y);
        if x = y then go (i + 1) ops a b
    | _ -> ()
  in
  go 0 ops (RT.run ops) (RS.run ops)

let strings =
  [ ""; "a"; "hello"; "\000\255\128"; String.make 40 'x'; "a$b ${c} $(d(e)f) \\$g \\\\ $"; "$nf"; "$boom" ]
  @ Platform.wide_strings

let substitutions =
  [ ""; "$"; "\\"; "\\\\"; "$$"; "a$"; "$-"; "$("; "${"; "$(a"; "${a"; "$a"; "$abc_9Z!"; "$(x y)"; "${x y}";
    "$(a(b)c)"; "$(a{b)c}"; "${a(b}c)"; "${a{b}c}"; "\\$a"; "\\\\$a"; "a\\b"; "end\\"; "$1$2"; "$_";
    "$é"; "$nf"; "x$(nf)y"; "x$boom y"; "$(boom)"; "pre $a mid ${b} post $(c) \\$d \\e \\" ]
  @ List.map (fun w -> w ^ "$a" ^ w ^ "$(" ^ w ^ ")\\" ^ w) Platform.wide_strings

let offsets = [ -1; 0; 1; 2; 5; 100 ]
let lengths = [ -1; 0; 1; 3; 100 ]
let int_values = [ 0; 1; 127; 128; 255; 256; 300; 0x7fff; 0x8000; 0xffff ]
let int16_values = int_values @ [ -1; -2; -32768; 65536; 70000; 0x12345; max_int; min_int ]
let int32_values = [ 0l; 1l; -1l; 0x12345678l; Int32.max_int; Int32.min_int; 0x80l ]

let int64_values =
  [ 0L; 1L; -1L; 0x0102030405060708L; Int64.max_int; Int64.min_int; 0x80000000L; 0xffffffffL ]

let uchars =
  [ 0; 0x41; 0x7f; 0x80; 0xe9; 0x7ff; 0x800; 0x20ac; 0xd7ff; 0xe000; 0xfffd; 0xffff; 0x10000; 0x1f600;
    0x10ffff ]

let state = ref 7

let pick l =
  state := ((!state * 1103515245) + 12345) land 0x3fffffff;
  List.nth l ((!state lsr 4) mod List.length l)

let index n = pick (List.init n Fun.id)

let random_op () =
  match index 23 with
  | 0 -> Add_char (pick ([ 'a'; '\000'; '\255'; '$' ] @ List.filter_map (fun s -> if s = "" then None else Some s.[0]) strings))
  | 1 -> Add_string (pick strings)
  | 2 -> Add_substring (pick strings, pick offsets, pick lengths)
  | 3 -> Add_bytes (pick strings)
  | 4 -> Add_subbytes (pick strings, pick offsets, pick lengths)
  | 5 -> Add_utf_8 (pick uchars)
  | 6 -> Add_utf_16le (pick uchars)
  | 7 -> Add_utf_16be (pick uchars)
  | 8 -> let f = index 8 in Add_int (f, pick (if f < 2 then int_values else int16_values))
  | 9 -> Add_int32 (index 3, pick int32_values)
  | 10 -> Add_int64 (index 3, pick int64_values)
  | 11 -> Add_seq (pick strings)
  | 12 -> if index 4 = 0 then Add_self else Add_other (pick strings)
  | 13 -> Add_substitute (pick substitutions)
  | 14 -> Truncate (pick [ -1; 0; 1; 3; 20; 1000 ])
  | 15 -> if index 6 = 0 then Clear else Reset
  | 16 -> Nth (pick [ -1; 0; 1; 2; 7; 50; 1000 ])
  | 17 -> Sub (pick offsets, pick lengths)
  | 18 -> Blit (pick offsets, pick [ 0; 3; 10; 200 ], pick offsets, pick lengths)
  | 19 -> To_bytes
  | 20 -> Seq_change (index 5)
  | _ -> Add_string (pick strings)

let fixed =
  List.concat
    [ List.map (fun s -> Add_string s) strings;
      List.concat_map (fun u -> [ Add_utf_8 u; Add_utf_16le u; Add_utf_16be u ]) uchars;
      List.concat_map (fun x -> List.init 8 (fun f -> Add_int (f, x))) int_values;
      List.concat_map (fun x -> List.init 6 (fun f -> Add_int (f + 2, x))) int16_values;
      List.concat_map (fun x -> List.init 3 (fun f -> Add_int32 (f, x))) int32_values;
      List.concat_map (fun x -> List.init 3 (fun f -> Add_int64 (f, x))) int64_values;
      [ Add_self; Add_self; Nth (-1); Nth 0; Nth 1000; To_bytes; Seq_change 0; Seq_change 1;
        Seq_change 2; Seq_change 3; Seq_change 4; Add_seq "abc"; Add_other "q"; Truncate 1001; Truncate 5 ];
      List.map (fun s -> Add_substitute s) substitutions ]

let () =
  List.iter
    (fun n -> compare_runs (Printf.sprintf "create %d" n) (Create n :: fixed))
    [ -5; 0; 1; 16 ];
  compare_runs "of_seq" (Of_seq "seed" :: fixed);
  List.iter
    (fun s -> compare_runs ("of_seq " ^ show s) [ Of_seq s; Seq_change 4; Seq_change 1 ])
    strings;
  for i = 1 to 300 do
    compare_runs (Printf.sprintf "random %d" i) (Create (index 40) :: List.init (1 + index 60) (fun _ -> random_op ()))
  done;
  List.iter
    (fun (s, o, l) ->
      List.iter
        (fun (d, n, dl) ->
          compare_runs
            (Printf.sprintf "ranges %S %d %d %d %d %d" s o l d n dl)
            [ Add_string s; Sub (o, l); Blit (o, n, d, l); Blit (o, n, d, dl); Add_substring (s, o, l);
              Add_subbytes (s, o, l); Nth o; Truncate o ])
        [ (0, 3, 3); (2, 5, 2); (-1, 5, 0); (4, 5, 2); (0, 0, 0) ])
    (List.concat_map (fun s -> List.concat_map (fun o -> List.map (fun l -> (s, o, l)) lengths) offsets)
       [ ""; "abcde"; "0123456789" ])

type case = { run : 'b. (module B with type t = 'b) -> 'b -> string }

let pair name { run } =
  same name
    (fun () -> let b = T.create 1 in T.add_string b "pre"; run (module T) b)
    (fun () -> let b = Stdlib.Buffer.create 1 in Stdlib.Buffer.add_string b "pre"; run (module Stdlib.Buffer) b)

let with_in path f =
  let ic = open_in_bin path in
  match f ic with
  | v -> close_in_noerr ic; v
  | exception e -> close_in_noerr ic; raise e

let channel_tests () =
  List.iter
    (fun data ->
      let path = Platform.prepare data in
      List.iter
        (fun n ->
          pair
            (Printf.sprintf "add_channel %S %d" (show data) n)
            { run = fun (type b) (module B : B with type t = b) (b : b) ->
              let r = result (fun () -> with_in path (fun ic -> B.add_channel b ic n)) in
              show_result (Result.map (fun () -> "()") r) ^ " " ^ B.contents b })
        [ -1; 0; 1; 3; 4; String.length data; String.length data + 1; 100 ])
    ([ ""; "abc"; "0123456789"; String.init 256 Char.chr ]
    @ (if Platform.native then [] else "boom12345" :: Platform.wide_strings));
  List.iter
    (fun s ->
      pair ("output_buffer " ^ show s) { run = fun (type b) (module B : B with type t = b) (b : b) ->
          B.add_string b s;
          B.add_int8 b 200;
          let path = Platform.prepare "" in
          let oc = open_out_bin path in
          B.output_buffer oc b;
          close_out oc;
          Platform.read_back path })
    strings

let known_differences () =
  List.iter
    (fun x ->
      let t = T.create 1 and s = Stdlib.Buffer.create 1 in
      T.add_int8 t x;
      Stdlib.Buffer.add_int8 s x;
      report (Printf.sprintf "add_int8 %d contents" x) (Ok (T.contents t)) (Ok (Stdlib.Buffer.contents s));
      report (Printf.sprintf "add_int8 %d nth tiny" x) (Ok (code (T.nth t 0))) (Ok (string_of_int (x land 0xffff)));
      report (Printf.sprintf "add_int8 %d nth stdlib" x) (Ok (code (Stdlib.Buffer.nth s 0))) (Ok (string_of_int x)))
    [ -1; -128; 65536; 70000 ]

let () =
  if not Platform.native then (
    pair "output_buffer stdout" { run = fun (type b) (module B : B with type t = b) (b : b) ->
        B.output_buffer stdout b; "" };
    pair "add_channel stdin" { run = fun (type b) (module B : B with type t = b) (b : b) ->
        B.add_channel b stdin 3; "" };
    pair "add_channel stdin 0" { run = fun (type b) (module B : B with type t = b) (b : b) ->
        B.add_channel b stdin 0; B.contents b };
    pair "add_channel stdin -1" { run = fun (type b) (module B : B with type t = b) (b : b) ->
        B.add_channel b stdin (-1); "" };
    known_differences ());
  Platform.install_fake_runtime ();
  channel_tests ();
  Printf.printf "tiny_buffer %s: %d checks, %d failures\n"
    (if Platform.native then "native" else "melange")
    !checks !failures;
  if !failures > 0 then exit 1
