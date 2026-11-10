module M = Tiny_md5.Digest
module D = Stdlib.Digest
module _ : module type of Stdlib.Digest = Tiny_md5.Digest
module _ : module type of Tiny_md5.Digest = Stdlib.Digest

let failures = ref 0
let checks = ref 0
let stdlib_bugs = ref 0
let result f = match f () with v -> Ok v | exception e -> Error (Platform.describe e)
let show = function Ok v -> String.escaped v | Error e -> "exception " ^ e

let short s =
  if String.length s > 40 then String.escaped (String.sub s 0 40) ^ "..." else String.escaped s

let report name ours expected =
  incr checks;
  if ours <> expected then (
    incr failures;
    Printf.printf "FAIL %s\n  ours:     %s\n  expected: %s\n" name (show ours) (show expected))

let same name ours stdlib = report name (result ours) (result stdlib)
let same_any name to_s ours stdlib = same name (fun () -> to_s (ours ())) (fun () -> to_s (stdlib ()))
let bool b = string_of_bool b

let top_m : (module D.S) = (module struct include M let hash_length = 16 end)
let top_d : (module D.S) = (module struct include D let hash_length = 16 end)

let modules =
  [ ("Digest", true, top_m, top_d);
    ("MD5", true, (module M.MD5 : D.S), (module D.MD5 : D.S));
    ("BLAKE128", false, (module M.BLAKE128 : D.S), (module D.BLAKE128 : D.S));
    ("BLAKE256", false, (module M.BLAKE256 : D.S), (module D.BLAKE256 : D.S));
    ("BLAKE512", false, (module M.BLAKE512 : D.S), (module D.BLAKE512 : D.S)) ]

let check_string (name, _, (module A : D.S), (module B : D.S)) s =
  let n = Printf.sprintf "%s.string %S" name (short s) in
  same n (fun () -> A.string s) (fun () -> B.string s);
  same (n ^ " bytes") (fun () -> A.bytes (Bytes.of_string s)) (fun () -> B.bytes (Bytes.of_string s));
  same (n ^ " to_hex") (fun () -> A.to_hex (A.string s)) (fun () -> B.to_hex (B.string s))

let check_sub (name, is_md5, (module A : D.S), (module B : D.S)) s ofs len =
  let valid = ofs >= 0 && len >= 0 && ofs <= String.length s - len in
  let fixed = is_md5 && valid && not Platform.native in
  let expected = if fixed then fun () -> B.string (String.sub s ofs len) else fun () -> B.substring s ofs len in
  let n = Printf.sprintf "%s.substring %S %d %d" name (short s) ofs len in
  report n (result (fun () -> A.substring s ofs len)) (result expected);
  let b = Bytes.of_string s in
  let expected_b = if fixed then expected else fun () -> B.subbytes b ofs len in
  report (n ^ " subbytes") (result (fun () -> A.subbytes b ofs len)) (result expected_b);
  if fixed && result (fun () -> B.substring s ofs len) <> result expected then incr stdlib_bugs;
  if fixed && result (fun () -> B.subbytes b ofs len) <> result expected then incr stdlib_bugs

let check_hex (name, _, (module A : D.S), (module B : D.S)) h =
  same (Printf.sprintf "%s.of_hex %S" name (short h)) (fun () -> A.of_hex h) (fun () -> B.of_hex h)

let check_to_hex (name, _, (module A : D.S), (module B : D.S)) d =
  same (Printf.sprintf "%s.to_hex %S" name (short d)) (fun () -> A.to_hex d) (fun () -> B.to_hex d)

let check_pairs (name, _, (module A : D.S), (module B : D.S)) ds =
  List.iter
    (fun a ->
      List.iter
        (fun b ->
          same_any (name ^ ".compare") string_of_int (fun () -> A.compare a b) (fun () -> B.compare a b);
          same_any (name ^ ".equal") bool (fun () -> A.equal a b) (fun () -> B.equal a b))
        ds)
    ds

let rfc1321 =
  [ ("", "d41d8cd98f00b204e9800998ecf8427e");
    ("a", "0cc175b9c0f1b6a831c399e269772661");
    ("abc", "900150983cd24fb0d6963f7d28e17f72");
    ("message digest", "f96b697d7cb7938d525a2f31aaf161d0");
    ("abcdefghijklmnopqrstuvwxyz", "c3fcd3d76192e4007dfb496cca67e13b");
    ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789", "d174ab98d277d9f5a5611c2c9f419d9f");
    (String.concat "" (List.init 8 (fun _ -> "1234567890")), "57edf4a22be3c955ac49da2e2107b67a");
    ("The quick brown fox jumps over the lazy dog", "9e107d9d372bb6826bd81d3542a419d6");
    (String.make 1_000_000 'a', "7707d6ae4e027c70eea2a935c2296f21") ]

let rand n = String.init n (fun _ -> Char.chr (Random.int 256))

let hex_inputs len =
  let z = String.make (2 * len) '0' in
  [ ""; "00"; String.make ((2 * len) - 1) '0'; String.make ((2 * len) + 1) '0'; String.make (2 * len) 'g';
    String.make (2 * len) 'F'; z; String.sub z 0 ((2 * len) - 2) ^ "0x"; String.sub z 0 ((2 * len) - 1) ^ " ";
    "/" ^ String.sub z 1 ((2 * len) - 1); ":" ^ String.sub z 1 ((2 * len) - 1);
    "@" ^ String.sub z 1 ((2 * len) - 1); "G" ^ String.sub z 1 ((2 * len) - 1);
    "`" ^ String.sub z 1 ((2 * len) - 1); "\255" ^ String.sub z 1 ((2 * len) - 1) ]

let pure_tests ((name, _, (module A : D.S), (module B : D.S)) as m) ~full =
  same_any (name ^ ".hash_length") string_of_int (fun () -> A.hash_length) (fun () -> B.hash_length);
  let len = B.hash_length in
  Random.init 42;
  let lengths = if full then List.init 301 Fun.id @ [ 511; 512; 513; 4095; 4096; 65536; 100_003 ] else [ 0; 1; 63; 64; 65; 200 ] in
  List.iter (fun n -> check_string m (rand n); check_string m (String.make n '\255')) lengths;
  if full then for c = 0 to 255 do check_string m (String.make 1 (Char.chr c)) done;
  List.iter (fun (s, _) -> check_string m s) rfc1321;
  List.iter (check_string m) Platform.wide_strings;
  let s = rand 200 in
  for ofs = -2 to 202 do
    List.iter (fun l -> check_sub m s ofs l) [ -1; 0; 1; 2; 55; 56; 63; 64; 65; 120; 199; 200; 201 ]
  done;
  List.iter
    (fun (s, ofs, l) -> check_sub m s ofs l)
    [ ("", 0, 0); ("", 0, 1); ("", 1, 0); ("", -1, 0); ("abc", max_int, 1); ("abc", 1, max_int);
      ("abc", min_int, 1); ("abc", 1, min_int); ("abc", 3, 0); ("abc", 4, 0); ("abc", 0, 3); ("abc", 2, 1) ];
  List.iter (fun w -> for ofs = 0 to 2 do check_sub m w ofs (String.length w - ofs) done) Platform.wide_strings;
  List.iter (check_to_hex m)
    ([ ""; "short"; String.make (len - 1) 'x'; String.make (len + 1) 'x'; String.make (2 * len) 'x';
       String.make len '\000'; String.make len '\255' ]
    @ List.concat_map
        (fun w -> if String.length w = 1 then [ w ^ String.make (len - 1) 'a'; String.make (len - 1) 'a' ^ w ] else [])
        Platform.wide_strings);
  for n = 0 to 50 do
    let h = String.init (2 * len) (fun _ -> "0123456789abcdef".[Random.int 16]) in
    check_hex m h;
    check_hex m (String.uppercase_ascii h);
    check_to_hex m (String.init len (fun i -> Char.chr (((n * 31) + (i * 97)) land 255)))
  done;
  List.iter (check_hex m) (hex_inputs len);
  List.iter (fun w -> check_hex m (w ^ String.make ((2 * len) - 1) '0')) Platform.wide_strings;
  check_pairs m (List.init 12 (fun n -> String.init len (fun i -> Char.chr ((n * 37 + i * (n mod 3)) land 255))))

let fuzz ((_, is_md5, (module A : D.S), (module B : D.S)) as m) =
  Random.init 7;
  for _ = 1 to 3000 do
    let s = rand (Random.int 300) in
    let ofs = Random.int (String.length s + 3) - 1 and len = Random.int (String.length s + 3) - 1 in
    check_sub m s ofs len;
    if is_md5 then (
      let d = A.string s in
      let h = Bytes.of_string (A.to_hex d) in
      Bytes.set h (Random.int (Bytes.length h)) (Char.chr (Random.int 256));
      check_hex m (Bytes.to_string h);
      check_to_hex m d)
  done

let with_in path f =
  let ic = open_in_bin path in
  match f ic with
  | v -> close_in_noerr ic; v
  | exception e -> close_in_noerr ic; raise e

let file_error ((name, _, (module A : D.S), (module B : D.S)) : string * bool * (module D.S) * (module D.S)) =
  let p = Platform.prepare "boom" in
  let run file =
    let c = Platform.closes () in
    let r = result (fun () -> file p) in
    show r ^ " closes " ^ string_of_int (Platform.closes () - c)
  in
  same (name ^ " file error") (fun () -> run A.file) (fun () -> run B.file)

let io_tests (((name, _, (module A : D.S), (module B : D.S)) as m) : string * bool * (module D.S) * (module D.S)) =
  Random.init 3;
  List.iter
    (fun contents ->
      let p = Platform.prepare contents in
      let l = String.length contents in
      let n = Printf.sprintf "%s %d bytes" name l in
      same (n ^ " file") (fun () -> A.file p) (fun () -> B.file p);
      List.iter
        (fun k ->
          same (Printf.sprintf "%s channel %d" n k)
            (fun () -> with_in p (fun ic -> A.channel ic k))
            (fun () -> with_in p (fun ic -> B.channel ic k)))
        [ -1; -5; 0; 1; 3; 1000; 4096; 4097; l; l + 1 ];
      same (n ^ " input") (fun () -> with_in p A.input) (fun () -> with_in p B.input);
      same (n ^ " input twice")
        (fun () -> with_in p (fun ic -> let a = A.input ic in a ^ A.input ic))
        (fun () -> with_in p (fun ic -> let a = B.input ic in a ^ B.input ic)))
    [ ""; "abc"; rand 15; rand 16; rand 17; rand 64; rand 4096; rand 10_000 ];
  file_error m;
  report (name ^ " file works") (Result.map (fun _ -> "") (result (fun () -> A.file (Platform.prepare "abc")))) (Ok "");
  same (name ^ " file missing") (fun () -> A.file "/nonexistent/md5test") (fun () -> B.file "/nonexistent/md5test");
  let out f =
    let p = Platform.prepare "" in
    let oc = open_out_bin p in
    f oc;
    close_out oc;
    Platform.read_back p
  in
  List.iter
    (fun d -> same (name ^ " output") (fun () -> out (fun oc -> A.output oc d)) (fun () -> out (fun oc -> B.output oc d)))
    [ ""; "abc"; String.make 16 '\255'; rand 64 ]

let unpolyfilled ((name, _, (module A : D.S), (module B : D.S)) : string * bool * (module D.S) * (module D.S)) =
  same (name ^ " channel stdin") (fun () -> A.channel stdin 3) (fun () -> B.channel stdin 3);
  same (name ^ " channel stdin -1") (fun () -> A.channel stdin (-1)) (fun () -> B.channel stdin (-1));
  same (name ^ " input stdin") (fun () -> A.input stdin) (fun () -> B.input stdin);
  same (name ^ " file") (fun () -> A.file "x") (fun () -> B.file "x")

let () =
  List.iter
    (fun (s, h) ->
      report ("rfc1321 tiny " ^ short s) (result (fun () -> M.to_hex (M.string s))) (Ok h);
      report ("rfc1321 stdlib " ^ short s) (result (fun () -> D.to_hex (D.string s))) (Ok h);
      report ("rfc1321 tiny MD5 " ^ short s) (result (fun () -> M.MD5.to_hex (M.MD5.string s))) (Ok h))
    rfc1321;
  report "from_hex" (result (fun () -> M.from_hex "d41d8cd98f00b204e9800998ecf8427e")) (Ok (D.string ""));
  List.iter
    (fun h -> same ("from_hex " ^ short h) (fun () -> M.from_hex h) (fun () -> D.from_hex h))
    (hex_inputs 16);
  List.iter (fun ((name, _, _, _) as m) -> pure_tests m ~full:(name = "Digest" || name = "MD5")) modules;
  List.iter fuzz modules;
  if not Platform.native then List.iter unpolyfilled modules;
  Platform.install_fake_runtime ();
  List.iter (fun ((name, _, _, _) as m) -> pure_tests m ~full:(name = "BLAKE128")) modules;
  List.iter io_tests modules;
  Platform.restore_backtrace ();
  List.iter file_error modules;
  if !stdlib_bugs > 0 then
    Printf.printf "Stdlib.Digest substring/subbytes with ofs > 0 gave a wrong digest in %d calls\n" !stdlib_bugs;
  Printf.printf "%d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
