module M = Tiny_md5.Digest

let failures = ref 0
let checks = ref 0

let result f = match f () with v -> Ok v | exception e -> Error (Printexc.to_string e)

let same name ours stdlib =
  incr checks;
  if result ours <> result stdlib then (
    incr failures;
    Printf.printf "FAIL %s\n" name)

let check_string s =
  let name = Printf.sprintf "string %S" (if String.length s > 40 then String.sub s 0 40 else s) in
  same name (fun () -> M.string s) (fun () -> Stdlib.Digest.string s);
  same ("to_hex " ^ name) (fun () -> M.to_hex (M.string s)) (fun () -> Stdlib.Digest.to_hex (Stdlib.Digest.string s))

let native = match Sys.backend_type with Sys.Other _ -> false | _ -> true

let check_sub s ofs len =
  let valid = ofs >= 0 && len >= 0 && ofs <= String.length s - len in
  same
    (Printf.sprintf "substring len=%d ofs=%d len=%d" (String.length s) ofs len)
    (fun () -> M.substring s ofs len)
    (fun () ->
      if valid && not native then Stdlib.Digest.string (String.sub s ofs len)
      else Stdlib.Digest.substring s ofs len)

let check_hex h =
  same ("of_hex " ^ h) (fun () -> M.of_hex h) (fun () -> Stdlib.Digest.of_hex h);
  same ("from_hex " ^ h) (fun () -> M.from_hex h) (fun () -> Stdlib.Digest.from_hex h)

let () =
  Random.init 42;
  let rand n = String.init n (fun _ -> Char.chr (Random.int 256)) in
  List.iter check_string
    [ ""; "a"; "abc"; "message digest"; "abcdefghijklmnopqrstuvwxyz";
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
      String.concat "" (List.init 8 (fun _ -> "1234567890"));
      "The quick brown fox jumps over the lazy dog"; "\000"; "\255\255\255";
      "héllo wörld"; String.make 1_000_000 'a' ];
  for c = 0 to 255 do check_string (String.make 1 (Char.chr c)) done;
  for n = 0 to 300 do check_string (rand n); check_string (String.make n '\255') done;
  List.iter (fun n -> check_string (rand n)) [ 511; 512; 513; 4095; 4096; 65536; 100_003 ];
  let s = rand 200 in
  for ofs = -2 to 202 do
    List.iter (fun len -> check_sub s ofs len) [ -1; 0; 1; 55; 56; 64; 199; 200; 201 ]
  done;
  check_sub "" 0 0;
  check_sub "" 0 1;
  check_sub "abc" max_int 1;
  check_sub "abc" 1 max_int;
  List.iter
    (fun d -> same "to_hex invalid" (fun () -> M.to_hex d) (fun () -> Stdlib.Digest.to_hex d))
    [ ""; "short"; String.make 15 'x'; String.make 17 'x'; String.make 32 'x' ];
  for n = 0 to 50 do
    let d = Stdlib.Digest.string (rand n) in
    let h = Stdlib.Digest.to_hex d in
    check_hex h;
    check_hex (String.uppercase_ascii h)
  done;
  List.iter check_hex
    [ ""; "00"; String.make 31 '0'; String.make 33 '0'; String.make 32 'g';
      "0123456789abcdefABCDEF0123456789"; "0123456789abcdef0123456789abcde ";
      String.make 32 'F'; String.make 32 '0'; String.make 30 '0' ^ "0x" ];
  let ds = List.init 20 (fun n -> Stdlib.Digest.string (string_of_int (n mod 7))) in
  List.iter
    (fun a ->
      List.iter
        (fun b ->
          same "compare" (fun () -> M.compare a b) (fun () -> Stdlib.Digest.compare a b);
          same "equal" (fun () -> M.equal a b) (fun () -> Stdlib.Digest.equal a b))
        ds)
    ds;
  Printf.printf "%d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
