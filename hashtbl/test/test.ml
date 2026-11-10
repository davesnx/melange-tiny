module T = Tiny_hashtbl.Hashtbl

module type HT = sig
  type (!'a, !'b) t

  include
    module type of Stdlib__Hashtbl
      with type ('a, 'b) t := ('a, 'b) t
       and type statistics = Stdlib.Hashtbl.statistics
end

module _ : HT = T

module _ :
  module type of Tiny_hashtbl__Hashtbl with type ('a, 'b) t := ('a, 'b) Stdlib.Hashtbl.t =
  Stdlib.Hashtbl

let checks = ref 0

let fail fmt =
  Printf.ksprintf
    (fun s ->
      prerr_endline ("tiny_hashtbl test failed: " ^ s);
      exit 1)
    fmt

let check name ok =
  incr checks;
  if not ok then fail "%s" name

let same name tiny stdlib =
  incr checks;
  let rec go i a b =
    match (a, b) with
    | [], [] -> ()
    | x :: a, y :: b ->
        if String.equal x y then go (i + 1) a b
        else fail "%s, line %d\n  tiny:   %s\n  stdlib: %s" name i x y
    | x :: _, [] -> fail "%s, line %d only in tiny: %s" name i x
    | [], y :: _ -> fail "%s, line %d only in stdlib: %s" name i y
  in
  go 0 tiny stdlib

let trace f =
  let l = ref [] in
  f (fun s -> l := s :: !l);
  List.rev !l

let result f = match f () with s -> s | exception e -> "raised " ^ Printexc.to_string e
let show_int = string_of_int
let show_string s = Printf.sprintf "%S" s
let show_float f = Int64.to_string (Int64.bits_of_float f)
let show_list f l = "[" ^ String.concat ";" (List.map f l) ^ "]"
let show_pair f g (a, b) = "(" ^ f a ^ "," ^ g b ^ ")"
let show_option f = function None -> "None" | Some x -> "Some " ^ f x

type ('k, 't) ops = {
  create : int -> 't;
  clear : 't -> unit;
  reset : 't -> unit;
  copy : 't -> 't;
  add : 't -> 'k -> int -> unit;
  remove : 't -> 'k -> unit;
  find : 't -> 'k -> int;
  find_opt : 't -> 'k -> int option;
  find_all : 't -> 'k -> int list;
  replace : 't -> 'k -> int -> unit;
  mem : 't -> 'k -> bool;
  iter : ('k -> int -> unit) -> 't -> unit;
  filter_map_inplace : ('k -> int -> int option) -> 't -> unit;
  fold : ('k -> int -> int -> int) -> 't -> int -> int;
  length : 't -> int;
  stats : 't -> Stdlib.Hashtbl.statistics;
  to_seq : 't -> ('k * int) Seq.t;
  to_seq_keys : 't -> 'k Seq.t;
  to_seq_values : 't -> int Seq.t;
  add_seq : 't -> ('k * int) Seq.t -> unit;
  replace_seq : 't -> ('k * int) Seq.t -> unit;
  of_seq : ('k * int) Seq.t -> 't;
  rebuild : 't -> 't;
}

module Poly (H : HT) = struct
  let ops () =
    {
      create = (fun n -> H.create n);
      clear = H.clear;
      reset = H.reset;
      copy = H.copy;
      add = H.add;
      remove = H.remove;
      find = H.find;
      find_opt = H.find_opt;
      find_all = H.find_all;
      replace = H.replace;
      mem = H.mem;
      iter = H.iter;
      filter_map_inplace = H.filter_map_inplace;
      fold = H.fold;
      length = H.length;
      stats = H.stats;
      to_seq = H.to_seq;
      to_seq_keys = H.to_seq_keys;
      to_seq_values = H.to_seq_values;
      add_seq = H.add_seq;
      replace_seq = H.replace_seq;
      of_seq = H.of_seq;
      rebuild = (fun t -> H.rebuild ~random:false t);
    }
end

module Of_s (M : Stdlib.Hashtbl.S) = struct
  let ops : (M.key, int M.t) ops =
    {
      create = M.create;
      clear = M.clear;
      reset = M.reset;
      copy = M.copy;
      add = M.add;
      remove = M.remove;
      find = M.find;
      find_opt = M.find_opt;
      find_all = M.find_all;
      replace = M.replace;
      mem = M.mem;
      iter = M.iter;
      filter_map_inplace = M.filter_map_inplace;
      fold = M.fold;
      length = M.length;
      stats = M.stats;
      to_seq = M.to_seq;
      to_seq_keys = M.to_seq_keys;
      to_seq_values = M.to_seq_values;
      add_seq = M.add_seq;
      replace_seq = M.replace_seq;
      of_seq = M.of_seq;
      rebuild = M.copy;
    }
end

let lookup ops show t k =
  Printf.sprintf "%s:%s find=%s opt=%s mem=%b" (show k)
    (show_list show_int (ops.find_all t k))
    (result (fun () -> show_int (ops.find t k)))
    (show_option show_int (ops.find_opt t k))
    (ops.mem t k)

let observe ops show keys t =
  let b = Buffer.create 256 in
  let add s =
    Buffer.add_string b s;
    Buffer.add_char b ' '
  in
  let kv k v = show k ^ "=" ^ show_int v in
  add ("len " ^ show_int (ops.length t));
  add "iter";
  ops.iter (fun k v -> add (kv k v)) t;
  add "fold";
  add (show_int (ops.fold (fun k v n -> add (kv k v); n + 1) t 0));
  add "seq";
  Seq.iter (fun (k, v) -> add (kv k v)) (ops.to_seq t);
  add "keys";
  Seq.iter (fun k -> add (show k)) (ops.to_seq_keys t);
  add "values";
  Seq.iter (fun v -> add (show_int v)) (ops.to_seq_values t);
  let s = ops.stats t in
  add
    (Printf.sprintf "stats %d %d %d %s" s.num_bindings s.num_buckets s.max_bucket_length
       (show_list show_int (Array.to_list s.bucket_histogram)));
  List.iter (fun k -> add (lookup ops show t k)) keys;
  Buffer.contents b

let fuzz ops show keys steps seed emit =
  let keys = Array.of_list keys in
  let n = Array.length keys in
  let state = ref seed in
  let rand m =
    state := ((!state * 1103515245) + 12345) land 0x3fffffff;
    !state mod m
  in
  let t = ref (ops.create (rand 40 - 5)) in
  for step = 1 to steps do
    let k = keys.(rand n) in
    let name =
      match rand 100 with
      | r when r < 30 ->
          ops.add !t k step;
          "add"
      | r when r < 48 ->
          ops.replace !t k step;
          "replace"
      | r when r < 66 ->
          ops.remove !t k;
          "remove"
      | r when r < 70 ->
          ops.filter_map_inplace
            (fun _ v -> match v mod 4 with 0 -> None | 1 -> Some (v * 2) | _ -> Some v)
            !t;
          "filter_map_inplace"
      | r when r < 73 ->
          t := ops.copy !t;
          "copy"
      | r when r < 75 ->
          t := ops.rebuild !t;
          "rebuild"
      | r when r < 78 ->
          ops.add_seq !t (List.to_seq [ (k, step); (keys.(rand n), -step); (k, step + 1) ]);
          "add_seq"
      | r when r < 81 ->
          ops.replace_seq !t (List.to_seq [ (k, step); (keys.(rand n), -step); (k, step + 1) ]);
          "replace_seq"
      | r when r < 82 ->
          t := ops.of_seq (ops.to_seq !t);
          "of_seq"
      | r when r < 86 ->
          for i = 1 to 40 do
            ops.add !t keys.(rand n) ((step * 100) + i)
          done;
          "bulk"
      | 86 ->
          ops.clear !t;
          "clear"
      | 87 ->
          ops.reset !t;
          "reset"
      | _ -> "lookup"
    in
    emit (Printf.sprintf "%d %s %s" step name (show k));
    emit (observe ops show (Array.to_list keys) !t)
  done

let compare_poly name show keys steps =
  let module A = Poly (T) in
  let module B = Poly (Stdlib.Hashtbl) in
  List.iter
    (fun seed ->
      let name = Printf.sprintf "fuzz %s seed %d" name seed in
      same name
        (trace (fuzz (A.ops ()) show keys steps seed))
        (trace (fuzz (B.ops ()) show keys steps seed)))
    [ 1; 2; 3 ]

type c = P | Q | R | S
type r = { a : int; b : string; c : float }
type v = A | B of int | C of string * int | D of v list | E of { x : int; y : v option }

let show_c = function P -> "P" | Q -> "Q" | R -> "R" | S -> "S"
let show_r r = Printf.sprintf "{%d,%S,%s}" r.a r.b (show_float r.c)

let rec show_v = function
  | A -> "A"
  | B i -> "B " ^ show_int i
  | C (s, i) -> Printf.sprintf "C (%S,%d)" s i
  | D l -> "D " ^ show_list show_v l
  | E { x; y } -> Printf.sprintf "E {%d,%s}" x (show_option show_v y)

let show_poly = function `X -> "`X" | `Y i -> "`Y " ^ show_int i | `Z s -> "`Z " ^ s

let ints = [ 0; 1; -1; 2; max_int; min_int; 42; 1000000 ] @ List.init 60 (fun i -> (i * 7) - 100)

let strings =
  [
    "";
    "a";
    "A";
    "__proto__";
    "constructor";
    "toString";
    "0";
    "-0";
    "NaN";
    "\xc3\xa9";
    "\000";
    "a\000b";
    "\xf0\x9f\x98\x80";
    String.make 1000 'x';
    String.make 1000 'y';
  ]
  @ List.init 50 (fun i -> "key" ^ string_of_int i)

let floats =
  [
    0.;
    -0.;
    nan;
    -.nan;
    infinity;
    neg_infinity;
    1.5;
    -1.5;
    epsilon_float;
    1e300;
    3.;
    0.1;
    max_float;
    min_float;
    5e-324;
    4294967296.;
    -2147483648.;
  ]
  @ List.init 30 (fun i -> float_of_int i /. 4.)

let tuples = List.init 60 (fun i -> (i mod 7, string_of_int (i / 7)))
let records = List.init 50 (fun i -> { a = i mod 5; b = string_of_int (i / 5); c = float_of_int (i mod 3) })
let lists = [] :: [ 1 ] :: [ 1; 2 ] :: List.init 30 (fun i -> List.init 15 (fun j -> if j = 14 then i else j))

let variants =
  [ A; B 0; B 1; B (-1); C ("", 0); C ("a", 1); D []; D [ A ]; D [ A; B 1 ]; E { x = 1; y = None } ]
  @ List.init 30 (fun i -> E { x = i mod 4; y = Some (if i mod 2 = 0 then B i else D [ C ("x", i) ]) })

let nested =
  List.init 40 (fun i ->
      ( [ (i mod 3, float_of_int (i mod 2)); (i, -0.) ],
        if i mod 4 = 0 then None else Some (B (i mod 5)) ))

let show_nested (l, o) = show_pair (show_list (show_pair show_int show_float)) (show_option show_v) (l, o)
let int64s = [ 0L; 1L; -1L; Int64.max_int; Int64.min_int; 4294967296L; 4294967295L ] @ List.init 20 Int64.of_int

let () =
  compare_poly "ints" show_int ints 1500;
  compare_poly "strings" show_string strings 1500;
  compare_poly "floats" show_float floats 1500;
  compare_poly "chars" (fun c -> show_int (Char.code c)) (List.init 60 (fun i -> Char.chr (i * 4))) 800;
  compare_poly "bools" string_of_bool [ true; false ] 400;
  compare_poly "unit" (fun () -> "()") [ () ] 300;
  compare_poly "constructors" show_c [ P; Q; R; S ] 400;
  compare_poly "tuples" (show_pair show_int show_string) tuples 1200;
  compare_poly "records" show_r records 1200;
  compare_poly "lists" (show_list show_int) lists 1200;
  compare_poly "variants" show_v variants 1200;
  compare_poly "nested" show_nested nested 1000;
  compare_poly "int64" Int64.to_string int64s 800;
  compare_poly "options" (show_option (show_option show_int)) [ None; Some None; Some (Some 1); Some (Some 2) ] 400;
  compare_poly "polymorphic variants" show_poly ([ `X; `Z "a"; `Z "" ] @ List.init 20 (fun i -> `Y i)) 800

module Scenarios (H : HT) = struct
  let ints_table n =
    let t = H.create 16 in
    for i = 0 to n - 1 do
      H.add t i (i * 10)
    done;
    t

  let dump t = H.fold (fun k v acc -> Printf.sprintf "%d=%d" k v :: acc) t [] |> List.rev |> String.concat " "

  let show_stats (s : Stdlib.Hashtbl.statistics) =
    Printf.sprintf "%d %d %d %s" s.num_bindings s.num_buckets s.max_bucket_length
      (show_list show_int (Array.to_list s.bucket_histogram))

  let dump_s t = H.fold (fun k v acc -> Printf.sprintf "%s=%d" k v :: acc) t [] |> List.rev |> String.concat " "

  let shadowing emit =
    let t = H.create 4 in
    let obs () = emit (Printf.sprintf "%s | %s" (show_list show_int (H.find_all t "k")) (dump_s t)) in
    H.add t "k" 1;
    H.add t "k" 2;
    H.add t "j" 9;
    H.add t "k" 3;
    obs ();
    H.replace t "k" 4;
    obs ();
    H.remove t "k";
    obs ();
    emit (result (fun () -> show_int (H.find t "k")));
    H.remove t "k";
    H.remove t "k";
    obs ();
    emit (result (fun () -> show_int (H.find t "k")));
    emit (show_option show_int (H.find_opt t "k"));
    H.replace t "k" 5;
    H.add t "k" 6;
    obs ()

  let float_keys emit =
    let t = H.create 1 in
    let obs () =
      emit
        (String.concat " "
           (H.fold (fun k v acc -> (show_float k ^ "=" ^ show_int v) :: acc) t [])
        ^ " | "
        ^ String.concat " "
            (List.map
               (fun k -> show_float k ^ ":" ^ show_list show_int (H.find_all t k))
               [ nan; -.nan; 0.; -0. ]))
    in
    H.add t nan 1;
    H.add t nan 2;
    H.replace t (-0.) 3;
    obs ();
    H.replace t 0. 4;
    obs ();
    H.add t (-.nan) 5;
    H.replace t nan 6;
    obs ();
    H.remove t (-0.);
    obs ()

  let sizes emit =
    List.iter
      (fun n ->
        let t = H.create n in
        emit (Printf.sprintf "create %d: %s" n (show_stats (H.stats t)));
        for i = 0 to (3 * abs n) + 70 do
          H.replace t (i * 31) i;
          emit (show_stats (H.stats t))
        done;
        emit (dump t);
        H.reset t;
        emit (show_stats (H.stats t));
        H.add t 1 1;
        H.clear t;
        emit (show_stats (H.stats t)))
      [ -5; 0; 1; 15; 16; 17; 31; 32; 33; 100; 1000 ]

  let iter_adds emit =
    let t = ints_table 30 in
    H.iter
      (fun k v ->
        emit (Printf.sprintf "visit %d=%d" k v);
        if k < 10 then H.add t (k + 100) v)
      t;
    emit (dump t);
    emit (show_stats (H.stats t));
    let t = ints_table 30 in
    let n =
      H.fold
        (fun k v acc ->
          emit (Printf.sprintf "fold %d=%d" k v);
          if k < 8 then H.replace t (k + 200) v;
          if k = 20 then H.remove t 21;
          acc + 1)
        t 0
    in
    emit (show_int n);
    emit (dump t);
    let t = ints_table 30 in
    H.filter_map_inplace
      (fun k v ->
        emit (Printf.sprintf "fmi %d=%d" k v);
        if k < 6 then H.add t (k + 300) v;
        if k mod 3 = 0 then None else Some (v + 1))
      t;
    emit (dump t);
    emit (show_int (H.length t))

  let iter_exn emit =
    let t = ints_table 30 in
    (try H.iter (fun k _ -> if k = 5 then raise Exit) t with Exit -> emit "exit");
    (try ignore (H.fold (fun k _ acc -> if k = 7 then failwith "stop" else acc) t 0)
     with Failure s -> emit s);
    (try H.filter_map_inplace (fun k v -> if k = 9 then raise Not_found else Some v) t
     with Not_found -> emit "not found");
    let s = H.to_seq t in
    for i = 30 to 99 do
      H.add t i i
    done;
    emit (dump t);
    emit (String.concat " " (List.map (fun (k, v) -> Printf.sprintf "%d=%d" k v) (List.of_seq s)));
    H.iter
      (fun k _ ->
        if k = 3 then
          try
            H.iter
              (fun k' _ ->
                if k' = 4 then begin
                  for i = 1000 to 1100 do
                    H.add t i i
                  done;
                  raise Exit
                end)
              t
          with Exit -> emit "inner exit")
      t;
    for i = 2000 to 2300 do
      H.add t i i
    done;
    emit (dump t)

  let copy_in_iter emit =
    let t = ints_table 20 in
    let c = ref (H.create 1) in
    H.iter (fun k _ -> if k = 0 then c := H.copy t) t;
    let s = H.to_seq_keys !c in
    for i = 100 to 200 do
      H.add !c i i
    done;
    emit (show_list show_int (List.of_seq s));
    emit (dump !c);
    emit (dump t);
    H.reset !c;
    emit (show_stats (H.stats !c));
    let d = H.copy t in
    H.add d 0 (-1);
    emit (dump d);
    emit (dump t)

  let reset_in_traversal emit =
    let t = ints_table 200 in
    emit
      (result (fun () ->
           H.filter_map_inplace
             (fun k v ->
               if k = 0 then H.reset t;
               Some v)
             t;
           "done"));
    emit (dump t);
    emit (show_stats (H.stats t));
    let t = ints_table 200 in
    emit
      (result (fun () ->
           H.iter (fun k _ -> if k = 0 then H.reset t) t;
           "done"));
    emit (dump t);
    let t = ints_table 200 in
    H.iter (fun k _ -> if k = 1 then H.clear t) t;
    emit (dump t);
    emit (show_stats (H.stats t))

  let seq_lazy emit =
    let t = ints_table 40 in
    let s = H.to_seq t in
    let s = match s () with Seq.Cons (x, rest) -> emit (show_pair show_int show_int x); rest | Seq.Nil -> s in
    let keys = H.to_seq_keys t and values = H.to_seq_values t in
    for i = 40 to 200 do
      H.add t i i
    done;
    H.remove t 3;
    H.replace t 5 555;
    emit (show_list (show_pair show_int show_int) (List.of_seq s));
    emit (show_list show_int (List.of_seq keys));
    emit (show_list show_int (List.of_seq values));
    let u = H.of_seq (List.to_seq [ (1, 1); (2, 2); (1, 3); (3, 4); (2, 5) ]) in
    emit (dump u);
    H.add_seq u (List.to_seq [ (1, 6); (1, 7) ]);
    H.replace_seq u (List.to_seq [ (2, 8); (9, 9) ]);
    emit (dump u);
    emit (show_list show_int (H.find_all u 1))

  let rebuild emit =
    let t = H.create 100 in
    for i = 0 to 300 do
      H.add t (i * 17) i;
      if i mod 7 = 0 then H.add t (i * 17) (-i)
    done;
    let r = H.rebuild ~random:false t in
    emit (dump r);
    emit (show_stats (H.stats r));
    emit (dump t);
    H.reset r;
    emit (show_stats (H.stats r));
    let small = ints_table 3 in
    let r = H.rebuild small in
    emit (dump r);
    emit (show_stats (H.stats r))

  let empty emit =
    let t : (string, int) H.t = H.create 0 in
    emit (result (fun () -> show_int (H.find t "x")));
    emit (show_option show_int (H.find_opt t "x"));
    emit (show_list show_int (H.find_all t "x"));
    H.remove t "x";
    emit (show_int (H.length t));
    emit (show_int (H.fold (fun _ _ n -> n + 1) t 0));
    emit (show_stats (H.stats t));
    let o = H.create 2 in
    H.replace o "a" None;
    H.replace o "b" (Some None);
    emit (show_option (show_option (show_option show_int)) (H.find_opt o "a"));
    emit (show_option (show_option (show_option show_int)) (H.find_opt o "b"))

  let functional emit =
    let f x = x + 1 and g x = x + 2 in
    let t = H.create 1 in
    H.add t f 1;
    emit (result (fun () -> string_of_bool (H.mem t f)));
    emit (result (fun () -> string_of_bool (H.mem t g)));
    emit (result (fun () -> show_int (H.find t g)));
    emit (result (fun () -> H.replace t g 2; "replaced"));
    emit (show_int (H.length t))

  let hashes emit =
    let h x =
      emit
        (Printf.sprintf "%d %d %d %d %d %d %d" (H.hash x) (H.seeded_hash 1 x) (H.seeded_hash 123456 x)
           (H.hash_param 1 1 x) (H.hash_param 3 5 x) (H.hash_param 1000 1000 x)
           (H.seeded_hash_param 2 7 99 x))
    in
    List.iter h ints;
    List.iter h strings;
    List.iter h floats;
    List.iter h tuples;
    List.iter h records;
    List.iter h lists;
    List.iter h variants;
    List.iter h nested;
    List.iter h int64s;
    List.iter h [ `X; `Y 1; `Z "a" ];
    List.iter h [ None; Some 1 ];
    List.iter h [ 'a'; '\255' ];
    List.iter h [ [| 1; 2 |]; [||] ];
    h ();
    h true;
    h (ref 1)

  let all =
    [
      ("shadowing", shadowing);
      ("float keys", float_keys);
      ("sizes", sizes);
      ("iter adds", iter_adds);
      ("iter exceptions", iter_exn);
      ("copy in iter", copy_in_iter);
      ("reset in traversal", reset_in_traversal);
      ("seq laziness", seq_lazy);
      ("rebuild", rebuild);
      ("empty", empty);
      ("functional keys", functional);
      ("hashes", hashes);
    ]
end

let () =
  let module A = Scenarios (T) in
  let module B = Scenarios (Stdlib.Hashtbl) in
  List.iter2
    (fun (name, a) (_, b) -> same name (trace a) (trace b))
    A.all B.all

let equal_log = ref []

module Weak = struct
  type t = string

  let equal a b =
    equal_log := (a ^ "~" ^ b) :: !equal_log;
    String.equal a b

  let hash = String.length
end

module Seeded = struct
  type t = string

  let equal = String.equal
  let seeded_hash seed s = seed + (String.length s * 7) + if s = "" then 0 else Char.code s.[0]
end

module Fuzz_functor (M : Stdlib.Hashtbl.S with type key = string) = struct
  module O = Of_s (M)

  let run seed =
    equal_log := [];
    let l = trace (fuzz O.ops show_string strings 1200 seed) in
    l @ List.rev !equal_log
end

module Fuzz_seeded (M : Stdlib.Hashtbl.SeededS with type key = string) = Fuzz_functor (struct
  include M

  let create n = M.create ~random:false n
end)

let () =
  let module A = Fuzz_functor (T.Make (Weak)) in
  let module B = Fuzz_functor (Stdlib.Hashtbl.Make (Weak)) in
  let module C = Fuzz_seeded (T.MakeSeeded (Seeded)) in
  let module D = Fuzz_seeded (Stdlib.Hashtbl.MakeSeeded (Seeded)) in
  List.iter
    (fun seed ->
      same (Printf.sprintf "Make seed %d" seed) (A.run seed) (B.run seed);
      same (Printf.sprintf "MakeSeeded seed %d" seed) (C.run seed) (D.run seed))
    [ 1; 2 ]

let bindings t = T.fold (fun k v acc -> (k, v) :: acc) t [] |> List.sort compare
let order t = T.fold (fun k _ acc -> k :: acc) t []

let fill t =
  for i = 0 to 299 do
    T.add t i (i * 2);
    if i mod 5 = 0 then T.add t i (-i)
  done

let expected_bindings =
  let s = Stdlib.Hashtbl.create ~random:false 16 in
  for i = 0 to 299 do
    Stdlib.Hashtbl.add s i (i * 2);
    if i mod 5 = 0 then Stdlib.Hashtbl.add s i (-i)
  done;
  Stdlib.Hashtbl.fold (fun k v acc -> (k, v) :: acc) s [] |> List.sort compare

let random_tables random =
  List.init 6 (fun _ ->
      let t = if random then T.create ~random:true 16 else T.create 16 in
      fill t;
      check "random table bindings" (bindings t = expected_bindings);
      check "random table find_all" (T.find_all t 10 = [ -10; 20 ]);
      check "random table length" (T.length t = 360);
      let r = T.rebuild ~random:true t in
      check "random rebuild bindings" (bindings r = expected_bindings);
      check "random rebuild find_all" (T.find_all r 10 = [ -10; 20 ]);
      order t)

let some_differ l = List.exists (fun o -> o <> List.hd l) l

let () =
  check "not randomized at start" (not (T.is_randomized ()));
  check "random:true gives random orders" (some_differ (random_tables true));
  check "default is not random" (not (some_differ (random_tables false)));
  T.randomize ();
  check "randomized" (T.is_randomized ());
  check "after randomize, create is random" (some_differ (random_tables false));
  let module M = T.Make (Weak) in
  let module N = Stdlib.Hashtbl.Make (Weak) in
  let m = M.create 16 and n = N.create 16 in
  List.iteri
    (fun i s ->
      M.add m s i;
      N.add n s i)
    strings;
  check "Make ignores randomize"
    (M.fold (fun k v acc -> (k, v) :: acc) m [] = N.fold (fun k v acc -> (k, v) :: acc) n [])

let () = print_endline (Printf.sprintf "tiny_hashtbl: %d checks passed" !checks)
