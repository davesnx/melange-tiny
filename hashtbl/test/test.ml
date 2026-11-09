module T = Tiny_hashtbl.Hashtbl

let checks = ref 0

let check name ok =
  incr checks;
  if not ok then failwith ("tiny_hashtbl test failed: " ^ name)

let seed = ref 12345

let rand n =
  seed := (!seed * 1103515245 + 12345) land 0x3fffffff;
  !seed mod n

let tiny_list t = T.fold (fun k v acc -> (k, v) :: acc) t [] |> List.rev

let tiny_iter_list t =
  let l = ref [] in
  T.iter (fun k v -> l := (k, v) :: !l) t;
  List.rev !l

let std_sorted t =
  List.sort compare (Stdlib.Hashtbl.fold (fun k v acc -> (k, v) :: acc) t [])

let model_replace model k v =
  if List.exists (fun (k', _) -> compare k k' = 0) model then
    List.map (fun (k', v') -> if compare k k' = 0 then (k', v) else (k', v')) model
  else model @ [ (k, v) ]

let model_remove model k = List.filter (fun (k', _) -> compare k k' <> 0) model

let same_order a b =
  List.length a = List.length b && List.for_all2 (fun x y -> compare x y = 0) a b

let run_random name keys steps =
  let keys = Array.of_list keys in
  let values = [| None; Some 0; Some 1; Some (-1); Some max_int |] in
  let tiny = T.create (rand 20) in
  let std = Stdlib.Hashtbl.create 16 in
  let model = ref [] in
  for step = 1 to steps do
    let label op = Printf.sprintf "%s step %d %s" name step op in
    let k = keys.(rand (Array.length keys)) in
    (match rand 12 with
    | 0 | 1 | 2 | 3 ->
        let v = values.(rand (Array.length values)) in
        T.replace tiny k v;
        Stdlib.Hashtbl.replace std k v;
        model := model_replace !model k v
    | 4 | 5 ->
        T.remove tiny k;
        Stdlib.Hashtbl.remove std k;
        model := model_remove !model k
    | 6 -> check (label "find_opt") (T.find_opt tiny k = Stdlib.Hashtbl.find_opt std k)
    | 7 -> check (label "mem") (T.mem tiny k = Stdlib.Hashtbl.mem std k)
    | 8 ->
        let get f = match f () with v -> Ok v | exception Not_found -> Error () in
        check (label "find")
          (get (fun () -> T.find tiny k) = get (fun () -> Stdlib.Hashtbl.find std k))
    | 9 ->
        if rand 10 = 0 then begin
          T.clear tiny;
          Stdlib.Hashtbl.clear std;
          model := []
        end
    | 10 ->
        if rand 10 = 0 then begin
          T.reset tiny;
          Stdlib.Hashtbl.reset std;
          model := []
        end
    | _ -> check (label "fold sum") (T.fold (fun _ _ n -> n + 1) tiny 0 = Stdlib.Hashtbl.length std));
    check (label "length") (T.length tiny = Stdlib.Hashtbl.length std);
    check (label "bindings") (same_order (List.sort compare (tiny_list tiny)) (std_sorted std));
    check (label "fold order") (same_order (tiny_list tiny) !model);
    check (label "iter order") (same_order (tiny_iter_list tiny) !model)
  done

type c = A | B | C | D

let long = String.make 1000 'x'

let strings =
  [ ""; "a"; "A"; "__proto__"; "constructor"; "toString"; "hasOwnProperty";
    "valueOf"; "0"; "1"; "-0"; "NaN"; "undefined"; "null"; "\xc3\xa9";
    "\000"; "a\000b"; "\xf0\x9f\x98\x80"; long; "key " ]
  @ List.init 30 (fun i -> "key" ^ string_of_int i)

let ints = [ 0; 1; -1; 2; max_int; min_int; 42; 1000000 ] @ List.init 30 (fun i -> i * 7)
let floats = [ 0.; -0.; infinity; neg_infinity; 1.5; -1.5; epsilon_float; 1e300; 3. ]

let () =
  run_random "strings" strings 5000;
  run_random "ints" ints 5000;
  run_random "floats" floats 3000;
  run_random "chars" [ 'a'; 'b'; '\000'; '\255'; ' ' ] 2000;
  run_random "bools" [ true; false ] 1000;
  run_random "constructors" [ A; B; C; D ] 1000;
  run_random "unit" [ () ] 500

let () =
  let t = T.create 1 in
  let s = Stdlib.Hashtbl.create 1 in
  List.iter
    (fun (k, v) ->
      T.replace t k v;
      Stdlib.Hashtbl.replace s k v)
    [ (nan, 1); (nan, 2); (-0., 3); (0., 4) ];
  let result length find_opt = (length, find_opt nan, find_opt (-0.), find_opt 0.) in
  let tiny = result (T.length t) (T.find_opt t) in
  check "nan and zero keys" (tiny = (2, Some 2, Some 4, Some 4));
  if Sys.backend_type = Sys.Native then
    check "nan and zero keys as stdlib"
      (tiny = result (Stdlib.Hashtbl.length s) (Stdlib.Hashtbl.find_opt s))

let () =
  let t = T.create 0 in
  check "empty length" (T.length t = 0);
  check "empty find_opt" (T.find_opt t "x" = None);
  check "empty find" (match T.find t "x" with _ -> false | exception Not_found -> true);
  T.remove t "x";
  check "remove missing" (T.length t = 0);
  T.replace t "a" None;
  check "None value" (T.find_opt t "a" = Some None && T.mem t "a" && T.find t "a" = None);
  T.replace t "b" (Some None);
  check "Some None value" (T.find_opt t "b" = Some (Some None))

let () =
  let t = T.create 4 in
  List.iter (fun k -> T.replace t k k) [ "a"; "b"; "c" ];
  T.replace t "a" "a2";
  T.remove t "b";
  T.replace t "b" "b2";
  check "insertion order"
    (tiny_list t = [ ("a", "a2"); ("c", "c"); ("b", "b2") ]);
  T.reset t;
  T.replace t "z" "z";
  check "reuse after reset" (tiny_list t = [ ("z", "z") ])

let () =
  let n = 20000 in
  let t = T.create 1 in
  let std = Stdlib.Hashtbl.create 1 in
  for i = 0 to n - 1 do
    T.replace t (string_of_int i) i;
    Stdlib.Hashtbl.replace std (string_of_int i) i
  done;
  for i = 0 to n - 1 do
    if i mod 3 = 0 then begin
      T.remove t (string_of_int i);
      Stdlib.Hashtbl.remove std (string_of_int i)
    end
  done;
  check "large length" (T.length t = Stdlib.Hashtbl.length std);
  check "large bindings" (same_order (List.sort compare (tiny_list t)) (std_sorted std));
  check "large fold"
    (T.fold (fun _ v acc -> acc + v) t 0 = Stdlib.Hashtbl.fold (fun _ v acc -> acc + v) std 0)

let () = print_endline (Printf.sprintf "tiny_hashtbl: %d checks passed" !checks)
