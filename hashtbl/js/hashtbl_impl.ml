type ('a, 'b) t = {
  mutable size : int;
  mutable data : ('a, 'b) bucketlist array;
  seed : int;
  mutable initial_size : int;
}

and ('a, 'b) bucketlist =
  | Empty
  | Cons of { mutable key : 'a; mutable data : 'b; mutable next : ('a, 'b) bucketlist }

type statistics = Stdlib.Hashtbl.statistics = {
  num_bindings : int;
  num_buckets : int;
  max_bucket_length : int;
  bucket_histogram : int array;
}

module type HashedType = Stdlib.Hashtbl.HashedType
module type SeededHashedType = Stdlib.Hashtbl.SeededHashedType
module type S = Stdlib.Hashtbl.S
module type SeededS = Stdlib.Hashtbl.SeededS

let ongoing_traversal h = h.initial_size < 0
let flip_ongoing_traversal h = h.initial_size <- -h.initial_size
let randomized = ref false
let randomize () = randomized := true
let is_randomized () = !randomized

external math_random : unit -> float = "random" [@@mel.scope "Math"]

let random_seed () = int_of_float (math_random () *. 1073741824.)

let rec power_2_above x n =
  if x >= n then x else if x * 2 < x then x else power_2_above (x * 2) n

let create ?(random = !randomized) initial_size =
  let s = power_2_above 16 initial_size in
  let seed = if random then random_seed () else 0 in
  { initial_size = s; size = 0; seed; data = Array.make s Empty }

let clear h =
  if h.size > 0 then begin
    h.size <- 0;
    for i = 0 to Array.length h.data - 1 do
      h.data.(i) <- Empty
    done
  end

let reset h =
  let len = Array.length h.data in
  if len = abs h.initial_size then clear h
  else begin
    h.size <- 0;
    h.data <- Array.make (abs h.initial_size) Empty
  end

let copy_bucketlist = function
  | Empty -> Empty
  | Cons { key; data; next } ->
      let rec loop prec = function
        | Empty -> ()
        | Cons { key; data; next } ->
            let r = Cons { key; data; next } in
            (match prec with Empty -> () | Cons prec -> prec.next <- r);
            loop r next
      in
      let r = Cons { key; data; next } in
      loop r next;
      r

let copy h =
  let data = Array.make (Array.length h.data) Empty in
  for i = 0 to Array.length data - 1 do
    data.(i) <- copy_bucketlist h.data.(i)
  done;
  { h with data }
let length h = h.size

let insert_all_buckets indexfun inplace odata ndata =
  let nsize = Array.length ndata in
  let ndata_tail = Array.make nsize Empty in
  let rec insert_bucket = function
    | Empty -> ()
    | Cons { key; data; next } as cell ->
        let cell = if inplace then cell else Cons { key; data; next = Empty } in
        let nidx = indexfun key [@u] in
        (match ndata_tail.(nidx) with
        | Empty -> ndata.(nidx) <- cell
        | Cons tail -> tail.next <- cell);
        ndata_tail.(nidx) <- cell;
        insert_bucket next
  in
  for i = 0 to Array.length odata - 1 do
    insert_bucket odata.(i)
  done;
  if inplace then
    for i = 0 to nsize - 1 do
      match ndata_tail.(i) with Empty -> () | Cons tail -> tail.next <- Empty
    done

let resize index h =
  let odata = h.data in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize >= osize then begin
    let ndata = Array.make nsize Empty in
    let inplace = not (ongoing_traversal h) in
    h.data <- ndata;
    insert_all_buckets (fun [@u] key -> index h key [@u]) inplace odata ndata
  end

let iter f h =
  let rec do_bucket = function
    | Empty -> ()
    | Cons { key; data; next } ->
        f key data;
        do_bucket next
  in
  let old_trav = ongoing_traversal h in
  if not old_trav then flip_ongoing_traversal h;
  try
    let d = h.data in
    for i = 0 to Array.length d - 1 do
      do_bucket d.(i)
    done;
    if not old_trav then flip_ongoing_traversal h
  with exn when not old_trav ->
    flip_ongoing_traversal h;
    raise exn

let rec filter_map_inplace_bucket f h i prec = function
  | Empty -> (
      match prec with Empty -> h.data.(i) <- Empty | Cons c -> c.next <- Empty)
  | Cons ({ key; data; next } as c) as slot -> (
      match f key data with
      | None ->
          h.size <- h.size - 1;
          filter_map_inplace_bucket f h i prec next
      | Some data ->
          (match prec with Empty -> h.data.(i) <- slot | Cons c -> c.next <- slot);
          c.data <- data;
          filter_map_inplace_bucket f h i slot next)

let filter_map_inplace f h =
  let d = h.data in
  let old_trav = ongoing_traversal h in
  if not old_trav then flip_ongoing_traversal h;
  try
    for i = 0 to Array.length d - 1 do
      filter_map_inplace_bucket f h i Empty h.data.(i)
    done;
    if not old_trav then flip_ongoing_traversal h
  with exn when not old_trav ->
    flip_ongoing_traversal h;
    raise exn

let fold f h init =
  let rec do_bucket b accu =
    match b with
    | Empty -> accu
    | Cons { key; data; next } -> do_bucket next (f key data accu)
  in
  let old_trav = ongoing_traversal h in
  if not old_trav then flip_ongoing_traversal h;
  try
    let d = h.data in
    let accu = ref init in
    for i = 0 to Array.length d - 1 do
      accu := do_bucket d.(i) !accu
    done;
    if not old_trav then flip_ongoing_traversal h;
    !accu
  with exn when not old_trav ->
    flip_ongoing_traversal h;
    raise exn

let rec bucket_length accu = function
  | Empty -> accu
  | Cons { next; _ } -> bucket_length (accu + 1) next

let stats h =
  let mbl = ref 0 in
  for i = 0 to Array.length h.data - 1 do
    let l = bucket_length 0 h.data.(i) in
    if l > !mbl then mbl := l
  done;
  let mbl = !mbl in
  let histo = Array.make (mbl + 1) 0 in
  for i = 0 to Array.length h.data - 1 do
    let l = bucket_length 0 h.data.(i) in
    histo.(l) <- histo.(l) + 1
  done;
  {
    num_bindings = h.size;
    num_buckets = Array.length h.data;
    max_bucket_length = mbl;
    bucket_histogram = histo;
  }

let to_seq tbl =
  let tbl_data = tbl.data in
  let rec aux i buck () =
    match buck with
    | Empty -> if i = Array.length tbl_data then Seq.Nil else aux (i + 1) tbl_data.(i) ()
    | Cons { key; data; next } -> Seq.Cons ((key, data), aux i next)
  in
  aux 0 Empty

let rec seq_map f seq () =
  match seq () with Seq.Nil -> Seq.Nil | Seq.Cons (x, next) -> Seq.Cons (f x, seq_map f next)

let rec seq_iter f seq =
  match seq () with
  | Seq.Nil -> ()
  | Seq.Cons (x, next) ->
      f x;
      seq_iter f next

let to_seq_keys m = seq_map fst (to_seq m)
let to_seq_values m = seq_map snd (to_seq m)

let add_with index h key data =
  let i = index h key [@u] in
  let bucket = Cons { key; data; next = h.data.(i) } in
  h.data.(i) <- bucket;
  h.size <- h.size + 1;
  if h.size > Array.length h.data lsl 1 then resize index h

let rec remove_bucket equal h i key prec bucket =
  match bucket with
  | Empty -> bucket
  | Cons { key = k; next; _ } ->
      if equal k key [@u] then begin
        h.size <- h.size - 1;
        (match prec with Empty -> h.data.(i) <- next | Cons c -> c.next <- next);
        bucket
      end
      else remove_bucket equal h i key bucket next

let find_and_remove_with index equal h key =
  let i = index h key [@u] in
  match remove_bucket equal h i key Empty h.data.(i) with
  | Empty -> None
  | Cons { data; _ } -> Some data

let remove_with index equal h key =
  let i = index h key [@u] in
  ignore (remove_bucket equal h i key Empty h.data.(i))

let rec find_rec equal key = function
  | Empty -> raise Not_found
  | Cons { key = k; data; next } -> if equal key k [@u] then data else find_rec equal key next

let find_with index equal h key = find_rec equal key h.data.(index h key [@u])

let rec find_rec_opt equal key = function
  | Empty -> None
  | Cons { key = k; data; next } ->
      if equal key k [@u] then Some data else find_rec_opt equal key next

let find_opt_with index equal h key = find_rec_opt equal key h.data.(index h key [@u])

let find_all_with index equal h key =
  let[@tail_mod_cons] rec find_in_bucket = function
    | Empty -> []
    | Cons { key = k; data; next } ->
        if equal k key [@u] then data :: find_in_bucket next else find_in_bucket next
  in
  find_in_bucket h.data.(index h key [@u])

let rec retrieve_bucket equal key bucket =
  match bucket with
  | Empty -> bucket
  | Cons { key = k; next; _ } ->
      if equal k key [@u] then bucket else retrieve_bucket equal key next

let replace_bucket index h key i l data = function
  | Empty ->
      h.data.(i) <- Cons { key; data; next = l };
      h.size <- h.size + 1;
      if h.size > Array.length h.data lsl 1 then resize index h
  | Cons slot ->
      slot.key <- key;
      slot.data <- data

let find_and_replace_with index equal h key data =
  let i = index h key [@u] in
  let l = h.data.(i) in
  let bucket = retrieve_bucket equal key l in
  let old_data = match bucket with Cons { data; _ } -> Some data | Empty -> None in
  replace_bucket index h key i l data bucket;
  old_data

let replace_with index equal h key data =
  let i = index h key [@u] in
  let l = h.data.(i) in
  replace_bucket index h key i l data (retrieve_bucket equal key l)

let rec mem_in_bucket equal key = function
  | Empty -> false
  | Cons { key = k; next; _ } -> equal k key [@u] || mem_in_bucket equal key next

let mem_with index equal h key = mem_in_bucket equal key h.data.(index h key [@u])

module MakeSeeded (H : SeededHashedType) : SeededS with type key = H.t = struct
  type key = H.t
  type 'a hashtbl = (key, 'a) t
  type 'a t = 'a hashtbl

  let create = create
  let clear = clear
  let reset = reset
  let copy = copy
  let index = fun [@u] h key -> H.seeded_hash h.seed key land (Array.length h.data - 1)
  let equal = fun [@u] a b -> H.equal a b
  let add h key data = add_with index h key data
  let find_and_remove h key = find_and_remove_with index equal h key
  let remove h key = remove_with index equal h key
  let find h key = find_with index equal h key
  let find_opt h key = find_opt_with index equal h key
  let find_all h key = find_all_with index equal h key
  let find_and_replace h key data = find_and_replace_with index equal h key data
  let replace h key data = replace_with index equal h key data
  let mem h key = mem_with index equal h key
  let add_seq tbl i = seq_iter (fun (k, v) -> add tbl k v) i
  let replace_seq tbl i = seq_iter (fun (k, v) -> replace tbl k v) i

  let of_seq i =
    let tbl = create 16 in
    replace_seq tbl i;
    tbl

  let iter = iter
  let filter_map_inplace = filter_map_inplace
  let fold = fold
  let length = length
  let stats = stats
  let to_seq = to_seq
  let to_seq_keys = to_seq_keys
  let to_seq_values = to_seq_values
end

module Make (H : HashedType) : S with type key = H.t = struct
  include MakeSeeded (struct
    type t = H.t

    let equal = H.equal
    let seeded_hash (_seed : int) x = H.hash x
  end)

  let create sz = create ~random:false sz

  let of_seq i =
    let tbl = create 16 in
    replace_seq tbl i;
    tbl
end

external seeded_hash_param : int -> int -> int -> 'a -> int = "caml_hash" [@@noalloc]

let hash x = seeded_hash_param 10 100 0 x
let hash_param n1 n2 x = seeded_hash_param n1 n2 0 x
let seeded_hash seed x = seeded_hash_param 10 100 seed x

let key_index =
 fun [@u] h key -> seeded_hash_param 10 100 h.seed key land (Array.length h.data - 1)

let compare_equal = fun [@u] a b -> compare a b = 0
let add h key data = add_with key_index h key data
let find_and_remove h key = find_and_remove_with key_index compare_equal h key
let remove h key = remove_with key_index compare_equal h key
let find h key = find_with key_index compare_equal h key
let find_opt h key = find_opt_with key_index compare_equal h key
let find_all h key = find_all_with key_index compare_equal h key
let find_and_replace h key data = find_and_replace_with key_index compare_equal h key data
let replace h key data = replace_with key_index compare_equal h key data
let mem h key = mem_with key_index compare_equal h key
let add_seq tbl i = seq_iter (fun (k, v) -> add tbl k v) i
let replace_seq tbl i = seq_iter (fun (k, v) -> replace tbl k v) i

let of_seq i =
  let tbl = create 16 in
  replace_seq tbl i;
  tbl

let rebuild ?(random = !randomized) h =
  let s = power_2_above 16 (Array.length h.data) in
  let seed = if random then random_seed () else h.seed in
  let h' = { size = h.size; data = Array.make s Empty; seed; initial_size = h.initial_size } in
  insert_all_buckets (fun [@u] key -> key_index h' key [@u]) false h.data h'.data;
  h'
