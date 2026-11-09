type ('a, 'b) t = { table : ('a, int * 'b) Stdlib.Hashtbl.t; mutable next : int }

let create n = { table = Stdlib.Hashtbl.create n; next = 0 }
let clear t = Stdlib.Hashtbl.clear t.table
let reset t = Stdlib.Hashtbl.reset t.table
let length t = Stdlib.Hashtbl.length t.table
let mem t k = Stdlib.Hashtbl.mem t.table k
let remove t k = Stdlib.Hashtbl.remove t.table k
let find t k = snd (Stdlib.Hashtbl.find t.table k)
let find_opt t k = Option.map snd (Stdlib.Hashtbl.find_opt t.table k)

let replace t k v =
  match Stdlib.Hashtbl.find_opt t.table k with
  | Some (i, _) -> Stdlib.Hashtbl.replace t.table k (i, v)
  | None ->
      t.next <- t.next + 1;
      Stdlib.Hashtbl.replace t.table k (t.next, v)

let bindings t =
  List.sort
    (fun (a, _, _) (b, _, _) -> Int.compare a b)
    (Stdlib.Hashtbl.fold (fun k (i, v) acc -> (i, k, v) :: acc) t.table [])

let iter f t = List.iter (fun (_, k, v) -> f k v) (bindings t)
let fold f t init = List.fold_left (fun acc (_, k, v) -> f k v acc) init (bindings t)
