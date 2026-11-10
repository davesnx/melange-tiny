module T = Tiny_random.Random
module _ : module type of Stdlib.Random = T
module _ : module type of T = Stdlib.Random

let failures = ref 0
let checks = ref 0

let check name ok =
  incr checks;
  if not ok then begin
    incr failures;
    Printf.printf "FAIL %s\n" name
  end

let same name a b =
  let rec first i = function
    | x :: xs, y :: ys -> if x = y then first (i + 1) (xs, ys) else Some (i, x, y)
    | [], [] -> None
    | x :: _, [] -> Some (i, x, "<end>")
    | [], y :: _ -> Some (i, "<end>", y)
  in
  match first 0 (a, b) with
  | None -> check name true
  | Some (i, x, y) -> check (Printf.sprintf "%s: at %d tiny %s stdlib %s" name i x y) false

let show_float f = Printf.sprintf "%h/%Ld" f (Int64.bits_of_float f)

let run f =
  match f () with
  | v -> v
  | exception Invalid_argument m -> "Invalid_argument " ^ m
  | exception e -> "exn " ^ Printexc.to_string e

let script_rng = Stdlib.Random.State.make [| 2024 |]
let pick a = a.(Stdlib.Random.State.int script_rng (Array.length a))

let int_bounds =
  [| 1; 2; 3; 7; 10; 55; 100; 1000; 65536; 1 lsl 20; (1 lsl 29) + 1; 0x3FFFFFFE;
     0x3FFFFFFF; 0x40000000; 0x40000001; 0x7FFFFFFE; 0x7FFFFFFF; max_int;
     max_int - 1; 0; -1; -1000; min_int; min_int + 1 |]

let int32_bounds =
  [| 1l; 2l; 3l; 10l; 1000l; 0x3FFFFFFFl; 0x40000000l; 0x40000001l;
     0x7FFFFFFEl; Int32.max_int; 0l; -1l; Int32.min_int; -1000l |]

let int64_bounds =
  [| 1L; 2L; 3L; 10L; 1000L; 0x3FFFFFFFL; 0x40000000L; 0xFFFFFFFFL;
     0x100000000L; 0x1000000000000000L; 0x3FFFFFFFFFFFFFFFL;
     0x4000000000000000L; Int64.max_int; Int64.pred Int64.max_int; 0L; -1L;
     Int64.min_int; -1000L |]

let float_bounds =
  [| 1.; 0.; -0.; -1.; 0.5; 5.; 1e-300; -1e-300; 1e308; -1e308; 4.9e-324;
     max_float; infinity; neg_infinity; nan |]

type op =
  | Bits
  | Int of int
  | Full_int of int
  | Int32 of int32
  | Int64 of int64
  | Float of float
  | Bool
  | Bits32
  | Bits64

let random_op () =
  let r () = Stdlib.Random.State.bits script_rng in
  match Stdlib.Random.State.int script_rng 13 with
  | 0 -> Bits
  | 1 -> Int (pick int_bounds)
  | 2 -> Int (1 + Stdlib.Random.State.int script_rng 0x3FFFFFFF)
  | 3 -> Full_int (pick int_bounds)
  | 4 -> Full_int (r () lor (r () lsl 30))
  | 5 -> Int32 (pick int32_bounds)
  | 6 -> Int32 (Int32.logor (Int32.of_int (r ())) (Int32.shift_left (Int32.of_int (r ())) 30))
  | 7 -> Int64 (pick int64_bounds)
  | 8 -> Int64 (Int64.logor (Int64.of_int (r ())) (Int64.shift_left (Int64.of_int (r ())) 30))
  | 9 -> Float (pick float_bounds)
  | 10 -> Bool
  | 11 -> Bits32
  | _ -> Bits64

let script n = List.init n (fun _ -> random_op ())
let seq_of op n = List.init n (fun _ -> op)

let all_single_ops =
  [ ("bits", [ Bits ]); ("bool", [ Bool ]); ("bits32", [ Bits32 ]); ("bits64", [ Bits64 ]) ]
  @ List.map (fun b -> (Printf.sprintf "int %d" b, [ Int b ])) (Array.to_list int_bounds)
  @ List.map (fun b -> (Printf.sprintf "full_int %d" b, [ Full_int b ])) (Array.to_list int_bounds)
  @ List.map (fun b -> (Printf.sprintf "int32 %ld" b, [ Int32 b ])) (Array.to_list int32_bounds)
  @ List.map (fun b -> (Printf.sprintf "int64 %Ld" b, [ Int64 b ])) (Array.to_list int64_bounds)
  @ List.map (fun b -> (Printf.sprintf "float %h" b, [ Float b ])) (Array.to_list float_bounds)

module Trace (R : module type of Stdlib.Random) = struct
  let global = function
    | Bits -> fun () -> string_of_int (R.bits ())
    | Int b -> fun () -> string_of_int (R.int b)
    | Full_int b -> fun () -> string_of_int (R.full_int b)
    | Int32 b -> fun () -> Int32.to_string (R.int32 b)
    | Int64 b -> fun () -> Int64.to_string (R.int64 b)
    | Float b -> fun () -> show_float (R.float b)
    | Bool -> fun () -> string_of_bool (R.bool ())
    | Bits32 -> fun () -> Int32.to_string (R.bits32 ())
    | Bits64 -> fun () -> Int64.to_string (R.bits64 ())

  let state s = function
    | Bits -> fun () -> string_of_int (R.State.bits s)
    | Int b -> fun () -> string_of_int (R.State.int s b)
    | Full_int b -> fun () -> string_of_int (R.State.full_int s b)
    | Int32 b -> fun () -> Int32.to_string (R.State.int32 s b)
    | Int64 b -> fun () -> Int64.to_string (R.State.int64 s b)
    | Float b -> fun () -> show_float (R.State.float s b)
    | Bool -> fun () -> string_of_bool (R.State.bool s)
    | Bits32 -> fun () -> Int32.to_string (R.State.bits32 s)
    | Bits64 -> fun () -> Int64.to_string (R.State.bits64 s)

  let globals ops = List.map (fun op -> run (global op)) ops
  let states s ops = List.map (fun op -> run (state s op)) ops

  let after_init seed ops =
    R.init seed;
    globals ops

  let after_full_init seed ops =
    R.full_init seed;
    globals ops

  let made seed ops = states (R.State.make seed) ops

  let copies seed ops =
    let s = R.State.make seed in
    let a = states s ops in
    let c = R.State.copy s in
    let b = states c ops in
    let d = states s ops in
    a @ ("|" :: b) @ ("|" :: d)

  let get_set seed ops =
    R.init seed;
    let a = globals ops in
    let saved = R.get_state () in
    let b = globals ops in
    let c = states saved ops in
    R.set_state saved;
    let d = globals ops in
    let e = states saved ops in
    R.set_state (R.State.make [| seed; 1 |]);
    let f = globals ops in
    List.concat [ a; "|" :: b; "|" :: c; "|" :: d; "|" :: e; "|" :: f ]

  let self_init () =
    let ok = ref true in
    for _ = 1 to 20 do
      R.self_init ();
      let x = R.bits () and i = R.int 10 and f = R.float 1. in
      let s = R.State.make_self_init () in
      let y = R.State.bits s and j = R.State.full_int s 0x7FFFFFFF in
      ok :=
        !ok && x >= 0 && x < 0x40000000 && i >= 0 && i < 10 && f >= 0. && f < 1.
        && y >= 0 && y < 0x40000000 && j >= 0 && j < 0x7FFFFFFF
    done;
    !ok
end

module Tiny = Trace (T)
module Std = Trace (Stdlib.Random)

let tiny_default = T.get_state ()
let std_default = Stdlib.Random.get_state ()

let () =
  let ops = script 2000 in
  T.set_state tiny_default;
  let a = Tiny.globals ops in
  Stdlib.Random.set_state std_default;
  same "default state globals" a (Std.globals ops);
  same "default state copy" (Tiny.states tiny_default ops) (Std.states std_default ops)

let int_seeds =
  [ 0; 1; -1; 2; 42; 55; 27182818; 0x3FFFFFFF; 0x40000000; 0x7FFFFFFF; max_int;
    min_int; max_int - 1; min_int + 1 ]
  @ List.init 30 (fun _ -> Stdlib.Random.State.bits script_rng lxor (Stdlib.Random.State.bits script_rng lsl 30))

let array_seeds =
  [ [||]; [| 0 |]; [| 1; 2; 3 |]; [| max_int; min_int; -1 |]; Array.init 54 (fun i -> i);
    Array.init 55 (fun i -> i * 7919); Array.init 56 (fun i -> -i);
    Array.init 200 (fun i -> i * i * 1000003) ]
  @ List.init 10 (fun n -> Array.init (n * 13) (fun _ -> Stdlib.Random.State.bits script_rng - 0x20000000))

let () =
  List.iter
    (fun seed ->
      List.iter
        (fun (name, op) ->
          let ops = seq_of (List.hd op) 300 in
          same (Printf.sprintf "init %d: %s" seed name) (Tiny.after_init seed ops)
            (Std.after_init seed ops))
        all_single_ops;
      let ops = script 3000 in
      same (Printf.sprintf "init %d: mixed" seed) (Tiny.after_init seed ops) (Std.after_init seed ops);
      same (Printf.sprintf "State.make [|%d|]: mixed" seed) (Tiny.made [| seed |] ops)
        (Std.made [| seed |] ops))
    int_seeds;
  List.iteri
    (fun i seed ->
      let ops = script 2000 in
      same (Printf.sprintf "full_init #%d: mixed" i) (Tiny.after_full_init seed ops)
        (Std.after_full_init seed ops);
      same (Printf.sprintf "State.make #%d: mixed" i) (Tiny.made seed ops) (Std.made seed ops);
      let ops = script 300 in
      same (Printf.sprintf "State.copy #%d" i) (Tiny.copies seed ops) (Std.copies seed ops))
    array_seeds;
  List.iter
    (fun seed ->
      let ops = script 300 in
      same (Printf.sprintf "get_state set_state %d" seed) (Tiny.get_set seed ops) (Std.get_set seed ops))
    int_seeds;
  List.iter
    (fun seed ->
      let ops = script 500 in
      same "tiny state with stdlib functions" (Std.made seed ops) (Std.states (T.State.make seed) ops);
      same "stdlib state with tiny functions" (Tiny.made seed ops)
        (Tiny.states (Stdlib.Random.State.make seed) ops))
    array_seeds;
  check "self_init range" (Tiny.self_init ());
  check "stdlib self_init range" (Std.self_init ())

let () =
  Printf.printf "%d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
