module P = Tiny_printf.Printf

let checks = ref 0
let failures = ref 0

let check name expected actual =
  incr checks;
  if expected <> actual then begin
    incr failures;
    print_endline
      (name ^ ": expected " ^ String.escaped expected ^ ", got "
     ^ String.escaped actual)
  end

let ints =
  [ 0; 1; -1; 2; 7; 9; 10; 15; 16; 17; 31; 32; 99; 100; 127; 128; 255; 256;
    1000; 4095; 4096; 65535; 65536; 1_000_000; 16_777_215; 123_456_789;
    0x7fff_ffff; -0x8000_0000; -2; -15; -16; -255; -256; -65536;
    -123_456_789; max_int; min_int; max_int - 1; min_int + 1 ]

let floats =
  [ 0.; -0.; 1.; -1.; 2.; 10.; 100.; 0.1; 0.2; 0.3; 0.5; 1.5; 2.5; -2.5;
    1. /. 3.; 2. /. 3.; -1. /. 3.; 3.14159265358979; 2.718281828459045;
    1e-5; 1e-4; 0.000123456; 1e-7; 1.5e-10; 123456.; 1234567.; 999999.5;
    9.9999999; 99.99; 0.95; 0.05; 1e15; 1e16; 1e17; 1e21; 1e22; 1e100;
    -1e100; 1e300; 1e-300; 5e-324; 2.2250738585072014e-308; max_float;
    min_float; -.max_float; epsilon_float; 4503599627370496.;
    9007199254740993.; 0.1 +. 0.2; 123.456; -0.001; 1e-310; nan; -.nan;
    infinity; neg_infinity; 1073741824.; 2147483648.; 4294967296. ]

let max_precision =
  match Sys.backend_type with Sys.Other _ -> 21 | _ -> 25

let random_floats =
  let state = ref 42 in
  let next () =
    state := (!state * 1103515245 + 12345) land 0x3fff_ffff;
    !state
  in
  List.init 300 (fun i ->
      let m = float_of_int (next ()) /. float_of_int 0x3fff_ffff in
      let e = (next () mod 80) - 40 in
      let sign = if i mod 3 = 0 then -1. else 1. in
      sign *. m *. (10. ** float_of_int e))

let shortest f =
  let rec go p g =
    let x = g p f in
    if p >= 17 || float_of_string x = f then x else go (p + 1) g
  in
  (go 15 (fun p f -> Printf.sprintf "%.*g" p f), go 15 P.float_g)

let () =
  List.iter
    (fun n ->
      let name = "int " ^ string_of_int n in
      check name (Printf.sprintf "%d" n) (P.int n);
      check name (Printf.sprintf "%x" n) (P.hex n);
      check name (Printf.sprintf "%X" n) (P.hex ~upper:true n);
      for w = 0 to 20 do
        check (name ^ " width " ^ string_of_int w) (Printf.sprintf "%0*x" w n)
          (P.hex ~width:w n);
        check (name ^ " width " ^ string_of_int w) (Printf.sprintf "%0*X" w n)
          (P.hex ~upper:true ~width:w n)
      done;
      check name (Printf.sprintf "%02X" n) (P.hex ~upper:true ~width:2 n);
      check name (Printf.sprintf "%04x" n) (P.hex ~width:4 n))
    ints;
  List.iter
    (fun f ->
      let name = "float " ^ Printf.sprintf "%h" f in
      for p = 0 to max_precision do
        check (name ^ " p " ^ string_of_int p) (Printf.sprintf "%.*g" p f)
          (P.float_g p f)
      done;
      let expected, actual = shortest f in
      check (name ^ " shortest") expected actual)
    (floats @ random_floats);
  List.iter
    (fun (p, f, expected) ->
      check ("float_g " ^ string_of_int p ^ " " ^ expected) expected (P.float_g p f))
    [ (22, 1e21, "1000000000000000000000");
      (25, 1e22, "10000000000000000000000");
      (23, -5.884410530221099319296e+21, "-5884410530221099319296");
      (21, 1e21, "1e+21") ];
  check "float_g negative"
    "Invalid_argument"
    (match P.float_g (-1) 1. with
     | _ -> "no exception"
     | exception Invalid_argument _ -> "Invalid_argument");
  for i = 0 to 255 do
    let c = Char.chr i in
    check ("char " ^ string_of_int i) (Printf.sprintf "%C" c) (P.char_literal c)
  done;
  let open P in
  check "s at d" (Printf.sprintf "%s at %d" "bad" 42)
    (sprintf (s ^^ lit " at " ^^ d) "bad" 42);
  check "expected C" (Printf.sprintf "expected %C" '"')
    (sprintf (lit "expected " ^^ conv char_literal) '"');
  check "percent" (Printf.sprintf "%%%02X" 7)
    (sprintf (lit "%" ^^ conv (hex ~upper:true ~width:2)) 7);
  check "unicode escape" (Printf.sprintf "\\u%04x" 31)
    (sprintf (lit "\\u" ^^ conv (hex ~width:4)) 31);
  check "empty" (Printf.sprintf "") (sprintf (lit ""));
  check "only args" (Printf.sprintf "%s%s%d" "a" "" 3)
    (sprintf (s ^^ s ^^ d) "a" "" 3);
  check "island"
    (Printf.sprintf "yuka island %s (%s): cannot load code: %s" "i1" "counter"
       "404")
    (sprintf
       (lit "yuka island " ^^ s ^^ lit " (" ^^ s ^^ lit "): cannot load code: "
      ^^ s)
       "i1" "counter" "404");
  check "nested ^^ left"
    (Printf.sprintf "%d-%d-%d" 1 2 3)
    (sprintf ((d ^^ lit "-" ^^ d) ^^ lit "-" ^^ d) 1 2 3);
  check "float conv" (Printf.sprintf "x=%.3g" 3.14159)
    (sprintf (lit "x=" ^^ conv (float_g 3)) 3.14159);
  check "ksprintf"
    (Printf.ksprintf String.uppercase_ascii "action %s: HTTP %d" "save" 500)
    (ksprintf String.uppercase_ascii (lit "action " ^^ s ^^ lit ": HTTP " ^^ d)
       "save" 500);
  let fmt = s ^^ lit "/" ^^ d in
  let partial = sprintf fmt "x" in
  check "partial application" (Printf.sprintf "%s/%d" "x" 1) (partial 1);
  check "partial application again" (Printf.sprintf "%s/%d" "x" 2) (partial 2);
  check "ksprintf result type" "7"
    (string_of_int (ksprintf String.length (lit "abc" ^^ d ^^ lit "efg") 4));
  print_endline
    (string_of_int !checks ^ " checks, " ^ string_of_int !failures ^ " failures");
  if !failures > 0 then exit 1
