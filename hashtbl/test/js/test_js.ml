module T = Tiny_hashtbl.Hashtbl

let checks = ref 0

let check name ok =
  incr checks;
  if not ok then failwith ("tiny_hashtbl melange test failed: " ^ name)

let same = List.equal String.equal
let show_option = function None -> "None" | Some v -> string_of_int v

module Run (H : sig
  type (!'a, !'b) t

  val create : ?random:bool -> int -> ('a, 'b) t
  val add : ('a, 'b) t -> 'a -> 'b -> unit
  val replace : ('a, 'b) t -> 'a -> 'b -> unit
  val find_and_remove : ('a, 'b) t -> 'a -> 'b option
  val find_and_replace : ('a, 'b) t -> 'a -> 'b -> 'b option
  val find_all : ('a, 'b) t -> 'a -> 'b list
  val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
end) =
struct
  let run show keys seed =
    let keys = Array.of_list keys in
    let state = ref seed in
    let rand m =
      state := ((!state * 1103515245) + 12345) land 0x3fffffff;
      !state mod m
    in
    let t = H.create 1 in
    List.init 3000 (fun step ->
        let k = keys.(rand (Array.length keys)) in
        let r =
          match rand 4 with
          | 0 -> H.add t k step; "add"
          | 1 -> H.replace t k step; "replace"
          | 2 -> "remove " ^ show_option (H.find_and_remove t k)
          | _ -> "replace " ^ show_option (H.find_and_replace t k step)
        in
        String.concat " "
          ((r :: List.map string_of_int (H.find_all t k))
          @ H.fold (fun k v acc -> (show k ^ "=" ^ string_of_int v) :: acc) t []))
end

module A = Run (T)
module B = Run (Stdlib.Hashtbl)

let () =
  List.iter
    (fun seed ->
      let ints = List.init 80 (fun i -> i * 3) in
      check "ints" (same (A.run string_of_int ints seed) (B.run string_of_int ints seed));
      let pairs = List.init 60 (fun i -> (i mod 5, string_of_int i)) in
      let show (a, b) = string_of_int a ^ b in
      check "pairs" (same (A.run show pairs seed) (B.run show pairs seed)))
    [ 1; 2; 3 ]

module Weak = struct
  type t = string

  let log = ref []

  let equal a b =
    log := (a ^ "~" ^ b) :: !log;
    String.equal a b

  let hash = String.length
end

module Functor_run (M : Stdlib.Hashtbl.S with type key = string) = struct
 let run () =
  Weak.log := [];
  let t = M.create 1 in
  let out =
    List.init 2000 (fun i ->
        let k = string_of_int (i * 7919 mod 300) in
        let r =
          match i mod 4 with
          | 0 -> M.add t k i; "add"
          | 1 -> show_option (M.find_and_replace t k i)
          | 2 -> show_option (M.find_and_remove t k)
          | _ -> M.replace t k i; "replace"
        in
        r ^ " " ^ String.concat "," (M.fold (fun k v acc -> (k ^ "=" ^ string_of_int v) :: acc) t []))
  in
  out @ !Weak.log
end

module TM = Functor_run (T.Make (Weak))
module SM = Functor_run (Stdlib.Hashtbl.Make (Weak))

let () =
  check "Make find_and_remove/find_and_replace" (same (TM.run ()) (SM.run ()));
  print_endline (Printf.sprintf "tiny_hashtbl melange: %d checks passed" !checks)
