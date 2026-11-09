module T = Tiny_random.Random

let failures = ref 0

let check name ok =
  if not ok then begin
    incr failures;
    Printf.printf "FAIL %s\n" name
  end

let outcome f =
  match f () with
  | v -> Ok v
  | exception Invalid_argument m -> Error m

let int_bounds =
  [ 1; 2; 3; 7; 10; 16; 100; 1000; 65536; 1 lsl 20; (1 lsl 29) + 1;
    0x3FFFFFFE; 0x3FFFFFFF ]

let bad_int_bounds = [ 0; -1; -1000; min_int; 0x40000000; 1 lsl 40; max_int ]

let float_bounds =
  [ 1.; 0.; -0.; -1.; 0.5; 5.; 1e-300; -1e-300; 1e308; -1e308; 4.9e-324 ]

let in_float_range b r =
  if b >= 0. then r >= 0. && r <= b else r <= 0. && r >= b

let () =
  Random.init 1;
  T.init 1;
  List.iter
    (fun b ->
      for _ = 1 to 2000 do
        let s = Random.int b and t = T.int b in
        check (Printf.sprintf "int %d stdlib range" b) (s >= 0 && s < b);
        check (Printf.sprintf "int %d range %d" b t) (t >= 0 && t < b)
      done)
    int_bounds;
  List.iter
    (fun b ->
      let s = outcome (fun () -> Random.int b) in
      let t = outcome (fun () -> T.int b) in
      check (Printf.sprintf "int %d exception" b) (s = t && Result.is_error t))
    bad_int_bounds;
  for _ = 1 to 10000 do
    let s = Random.bits () and t = T.bits () in
    check "bits stdlib range" (s >= 0 && s < 1 lsl 30);
    check "bits range" (t >= 0 && t < 1 lsl 30)
  done;
  List.iter
    (fun b ->
      for _ = 1 to 2000 do
        let s = Random.float b and t = T.float b in
        check (Printf.sprintf "float %g stdlib range" b) (in_float_range b s);
        check (Printf.sprintf "float %g range %g" b t) (in_float_range b t)
      done)
    float_bounds;
  check "float nan" (Float.is_nan (Random.float nan) && Float.is_nan (T.float nan));
  let inf_ok f = let r = f infinity in r = infinity || Float.is_nan r in
  check "float infinity" (inf_ok Random.float && inf_ok T.float)

let buckets draw n count =
  let a = Array.make n 0 in
  for _ = 1 to count do
    let i = draw () in
    a.(i) <- a.(i) + 1
  done;
  a

let near_uniform name draw n =
  let count = 200_000 in
  let a = buckets draw n count in
  let e = float_of_int count /. float_of_int n in
  let chi =
    Array.fold_left (fun acc o -> let d = float_of_int o -. e in acc +. (d *. d /. e)) 0. a
  in
  let limit = float_of_int (n - 1) +. (6. *. sqrt (2. *. float_of_int (n - 1))) in
  check (Printf.sprintf "%s chi2 %.1f < %.1f" name chi limit) (chi < limit)

let () =
  T.init 7;
  Random.init 7;
  List.iter
    (fun n ->
      near_uniform (Printf.sprintf "stdlib int %d" n) (fun () -> Random.int n) n;
      near_uniform (Printf.sprintf "int %d" n) (fun () -> T.int n) n)
    [ 2; 3; 10; 37; 256 ];
  near_uniform "bool" (fun () -> if T.bool () then 1 else 0) 2;
  near_uniform "bits low byte" (fun () -> T.bits () land 255) 256;
  near_uniform "bits high byte" (fun () -> T.bits () lsr 22) 256;
  near_uniform "float" (fun () -> int_of_float (T.float 1. *. 64.)) 64;
  near_uniform "int big bound" (fun () -> T.int 0x3FFFFFFF lsr 24) 64

let seq f n = List.init n (fun _ -> f ())

let () =
  List.iter
    (fun seed ->
      T.init seed;
      let a = seq T.bits 100 in
      T.init seed;
      let b = seq T.bits 100 in
      check (Printf.sprintf "init %d repeats" seed) (a = b);
      check
        (Printf.sprintf "init %d varies" seed)
        (List.length (List.sort_uniq compare a) > 95);
      Random.init seed;
      let c = seq Random.bits 100 in
      Random.init seed;
      check (Printf.sprintf "stdlib init %d repeats" seed) (c = seq Random.bits 100))
    [ 0; 1; -1; 42; 0x6C078965; 0xFFFFFFFF; max_int; min_int ];
  T.init 1;
  let a = seq T.bits 20 in
  T.init 2;
  check "seeds 1 and 2 differ" (a <> seq T.bits 20);
  if Sys.backend_type = Sys.Native then begin
    T.init 5;
    let a = seq T.bits 20 in
    T.init (5 + (1 lsl 32));
    check "high seed bits used on native" (a <> seq T.bits 20)
  end;
  T.self_init ();
  let a = seq T.bits 20 in
  T.self_init ();
  check "self_init differs" (a <> seq T.bits 20)

let () =
  if !failures > 0 then begin
    Printf.printf "%d failures\n" !failures;
    exit 1
  end
  else print_endline "ok"
