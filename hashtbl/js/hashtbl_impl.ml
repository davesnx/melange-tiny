type ('a, 'b) t

external make : unit -> ('a, 'b) t = "Map" [@@mel.new]
external clear : ('a, 'b) t -> unit = "clear" [@@mel.send]
external replace : ('a, 'b) t -> 'a -> 'b -> unit = "set" [@@mel.send]
external mem : ('a, 'b) t -> 'a -> bool = "has" [@@mel.send]
external remove : ('a, 'b) t -> 'a -> unit = "delete" [@@mel.send]
external length : ('a, 'b) t -> int = "size" [@@mel.get]
external get : ('a, 'b) t -> 'a -> 'b = "get" [@@mel.send]

external for_each : ('a, 'b) t -> (('b -> 'a -> unit)[@u]) -> unit = "forEach"
[@@mel.send]

let create _ = make ()
let reset = clear
let find_opt t k = if mem t k then Some (get t k) else None
let find t k = if mem t k then get t k else raise Not_found
let iter f t = for_each t (fun [@u] v k -> f k v)

let fold f t init =
  let acc = ref init in
  for_each t (fun [@u] v k -> acc := f k v !acc);
  !acc
