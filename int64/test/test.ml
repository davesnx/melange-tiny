module T = Tiny_int64.Int64
module S = Stdlib.Int64

let failures = ref 0
let checks = ref 0

let expect name got want =
  incr checks;
  if got <> want then begin
    incr failures;
    if !failures <= 30 then print_endline (name ^ ": got " ^ got ^ ", want " ^ want)
  end

let run f = try f () with
  | Failure m -> "Failure " ^ m
  | Division_by_zero -> "Division_by_zero"

let edges =
  [ "0"; "1"; "-1"; "2"; "-2"; "7"; "-7"; "10"; "255"; "256"; "65535";
    "2147483647"; "-2147483648"; "2147483648"; "-2147483649"; "4294967295";
    "4294967296"; "-4294967296"; "9007199254740991"; "9007199254740992";
    "9007199254740993"; "-9007199254740993"; "4611686018427387903";
    "-4611686018427387904"; "4611686018427387904"; "9223372036854775807";
    "-9223372036854775808"; "9223372036854775806"; "-9223372036854775807";
    "1000000007"; "1700000000000"; "-123456789012345678" ]

let generated =
  let x = ref 88172645463325252L in
  List.init 300 (fun i ->
      x := S.add (S.mul !x 6364136223846793005L) 1442695040888963407L;
      let v = if i mod 3 = 0 then S.shift_right !x (i mod 64) else !x in
      S.to_string v)

let values = edges @ generated
let pairs_of xs = List.concat_map (fun a -> List.map (fun b -> (a, b)) xs) xs
let ts x = T.to_string x

let check_unary s =
  let t = T.of_string s and v = S.of_string s in
  let u name f g = expect (name ^ " " ^ s) (ts (f t)) (S.to_string (g v)) in
  expect ("of_string " ^ s) (ts t) (S.to_string v);
  u "neg" T.neg S.neg;
  u "succ" T.succ S.succ;
  u "pred" T.pred S.pred;
  u "abs" T.abs S.abs;
  u "lognot" T.lognot S.lognot;
  expect ("to_int " ^ s) (string_of_int (T.to_int t)) (string_of_int (S.to_int v));
  expect ("to_float " ^ s)
    (S.to_string (S.bits_of_float (T.to_float t)))
    (S.to_string (S.bits_of_float (S.to_float v)));
  expect ("of_int to_int " ^ s)
    (ts (T.of_int (T.to_int t)))
    (S.to_string (S.of_int (S.to_int v)));
  for n = 0 to 63 do
    let sh name f g =
      expect
        (Printf.sprintf "%s %s %d" name s n)
        (ts (f t n)) (S.to_string (g v n))
    in
    sh "shift_left" T.shift_left S.shift_left;
    sh "shift_right" T.shift_right S.shift_right;
    sh "shift_right_logical" T.shift_right_logical S.shift_right_logical
  done;
  List.iter
    (fun n ->
      expect
        (Printf.sprintf "shift_left %s %d" s n)
        (ts (T.shift_left t n))
        (S.to_string (S.shift_left v (n land 63))))
    [ 64; 65; 127; -1; -63 ]

let check_binary (a, b) =
  let ta = T.of_string a and tb = T.of_string b in
  let va = S.of_string a and vb = S.of_string b in
  let bin name f g =
    expect
      (name ^ " " ^ a ^ " " ^ b)
      (run (fun () -> ts (f ta tb)))
      (run (fun () -> S.to_string (g va vb)))
  in
  bin "add" T.add S.add;
  bin "sub" T.sub S.sub;
  bin "mul" T.mul S.mul;
  bin "div" T.div S.div;
  bin "rem" T.rem S.rem;
  bin "logand" T.logand S.logand;
  bin "logor" T.logor S.logor;
  bin "logxor" T.logxor S.logxor;
  bin "min" T.min S.min;
  bin "max" T.max S.max;
  expect ("compare " ^ a ^ " " ^ b)
    (string_of_int (T.compare ta tb))
    (string_of_int (S.compare va vb));
  expect ("equal " ^ a ^ " " ^ b)
    (string_of_bool (T.equal ta tb))
    (string_of_bool (S.equal va vb));
  expect ("poly compare " ^ a ^ " " ^ b)
    (string_of_int (compare ta tb))
    (string_of_int (compare va vb));
  expect ("poly equal " ^ a ^ " " ^ b)
    (string_of_bool (ta = tb))
    (string_of_bool (va = vb))

let strings =
  [
    ("", None);
    ("-", None);
    ("+", None);
    ("_", None);
    ("0", Some "0");
    ("-0", Some "0");
    ("+0", Some "0");
    ("+5", Some "5");
    ("00012", Some "12");
    ("1_000_000", Some "1000000");
    ("_1", None);
    ("1_", Some "1");
    ("1__2", Some "12");
    ("0x", None);
    ("0x_1", None);
    ("0xff", Some "255");
    ("0XFF", Some "255");
    ("0xFfFf", Some "65535");
    ("-0x1", Some "-1");
    ("+0x10", Some "16");
    ("0x7fffffffffffffff", Some "9223372036854775807");
    ("0x8000000000000000", Some "-9223372036854775808");
    ("0xffffffffffffffff", Some "-1");
    ("0x10000000000000000", None);
    ("-0xffffffffffffffff", Some "1");
    ("-0x8000000000000000", Some "-9223372036854775808");
    ("0o17", Some "15");
    ("0O777", Some "511");
    ("0o8", None);
    ("0o1777777777777777777777", Some "-1");
    ("0o2000000000000000000000", None);
    ("0b101", Some "5");
    ("0B11", Some "3");
    ("0b2", None);
    ("0b1111111111111111111111111111111111111111111111111111111111111111", Some "-1");
    ("0b10000000000000000000000000000000000000000000000000000000000000000", None);
    ("0u18446744073709551615", Some "-1");
    ("0u18446744073709551616", None);
    ("-0u1", Some "-1");
    ("0U10", Some "10");
    ("9223372036854775807", Some "9223372036854775807");
    ("9223372036854775808", None);
    ("-9223372036854775808", Some "-9223372036854775808");
    ("-9223372036854775809", None);
    ("99999999999999999999", None);
    ("12a", None);
    ("a12", None);
    (" 1", None);
    ("1 ", None);
    ("1.5", None);
    ("1e3", None);
    ("--1", None);
    ("-+1", None);
    ("0xg", None);
    ("0x1g", None);
    ("\217\161", None);
    ("1\0002", None)
  ]

let check_string ~native (s, want) =
  let show = function None -> "None" | Some x -> "Some " ^ x in
  let want_exn =
    match want with None -> "Failure Int64.of_string" | Some v -> v
  in
  let name = String.escaped s in
  expect ("of_string " ^ name) (run (fun () -> ts (T.of_string s))) want_exn;
  expect ("of_string_opt " ^ name)
    (show (Option.map ts (T.of_string_opt s)))
    (show want);
  if native then
    expect ("stdlib of_string " ^ name)
      (run (fun () -> S.to_string (S.of_string s)))
      want_exn

let floats =
  [ 0.; -0.; 0.5; -0.5; 0.999; -0.999; 1.; -1.; 1.5; -1.5; 2.7; -2.7;
    123456.789; -123456.789; 2147483648.5; -2147483649.5; 4294967296.;
    9007199254740991.; 9007199254740992.; -9007199254740993.; 1e15 +. 0.25;
    1e18; -1e18; 4611686018427387904.; 9223372036854774784.;
    -9223372036854775808.; 1700000000000.123 ]

let check_float f =
  expect ("of_float " ^ string_of_float f)
    (ts (T.of_float f))
    (S.to_string (S.of_float f))

let check_constants () =
  List.iter
    (fun (name, t, v) -> expect name (ts t) (S.to_string v))
    [ ("zero", T.zero, S.zero); ("one", T.one, S.one);
      ("minus_one", T.minus_one, S.minus_one); ("max_int", T.max_int, S.max_int);
      ("min_int", T.min_int, S.min_int) ]

let () =
  let native = match Sys.backend_type with Sys.Other _ -> false | _ -> true in
  check_constants ();
  List.iter check_unary values;
  List.iter check_binary (pairs_of (edges @ List.filteri (fun i _ -> i < 40) generated));
  List.iter (check_string ~native) strings;
  List.iter check_float floats;
  List.iter
    (fun s ->
      let f = S.to_float (S.of_string s) in
      if f < 9223372036854775808. then check_float f)
    values;
  Printf.printf "%d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
