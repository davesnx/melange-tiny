(** The platform part of {!Tiny_hashtbl.Hashtbl}. Use {!Tiny_hashtbl.Hashtbl}. *)

type ('a, 'b) t
val create : int -> ('a, 'b) t
val clear : ('a, 'b) t -> unit
val reset : ('a, 'b) t -> unit
val replace : ('a, 'b) t -> 'a -> 'b -> unit
val find : ('a, 'b) t -> 'a -> 'b
val find_opt : ('a, 'b) t -> 'a -> 'b option
val mem : ('a, 'b) t -> 'a -> bool
val remove : ('a, 'b) t -> 'a -> unit
val length : ('a, 'b) t -> int
val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
