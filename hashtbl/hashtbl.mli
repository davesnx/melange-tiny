(** A small hash table with the stdlib [Hashtbl] names, for a web UI runtime.

    The subset is: [create], [clear], [reset], [replace], [find], [find_opt],
    [mem], [remove], [length], [iter] and [fold]. Each function has the same
    name, type and result as the stdlib function, except for the differences
    below.

    Two implementations share this interface:
    - [melange-tiny.hashtbl.native] (native, the default) wraps
      [Stdlib.Hashtbl] and adds insertion order.
    - [melange-tiny.hashtbl.js] (Melange) is a JavaScript [Map]. A
      [melange.emit] stanza must list it in its [libraries].

    Differences from [Stdlib.Hashtbl]:
    - Keys must be strings, ints, chars, bools, floats or constant
      constructors. On Melange, a JavaScript [Map] compares keys with
      SameValueZero, so other keys (tuples, records, lists, constructors with
      arguments, [Int64]) are found only by physical equality. For the allowed
      keys, equality is the same as [compare a b = 0], including [nan] equal to
      [nan] and [0.] equal to [-0.].
    - There is one binding per key. There is no [add], because [add] can hide
      an earlier binding. Use [replace]; when the key is not in the table,
      [replace] does what [add] does.
    - [iter] and [fold] visit bindings in insertion order. [replace] on a key
      that is in the table keeps its position. A key that was removed and then
      added again goes to the end. The stdlib order depends on the hash.
    - [create] has no [?random] argument. The size is a hint; Melange ignores
      it.
    - On Melange, a [-0.] key is stored and returned by [iter] and [fold] as
      [0.].
    - If [iter] or [fold] calls a function that adds or removes bindings in the
      same table, the result is unspecified, as in the stdlib. *)

type ('a, 'b) t
(** A table from keys of type ['a] to values of type ['b]. *)

val create : int -> ('a, 'b) t
(** [create n] is a new empty table. [n] is the expected number of bindings. *)

val clear : ('a, 'b) t -> unit
(** Remove all bindings. *)

val reset : ('a, 'b) t -> unit
(** Remove all bindings. The same as [clear]. *)

val replace : ('a, 'b) t -> 'a -> 'b -> unit
(** [replace t k v] binds [k] to [v]. It replaces the binding of [k] if there
    is one. *)

val find : ('a, 'b) t -> 'a -> 'b
(** [find t k] is the value bound to [k].
    @raise Not_found if [k] has no binding. *)

val find_opt : ('a, 'b) t -> 'a -> 'b option
(** [find_opt t k] is [Some v] if [k] is bound to [v], else [None]. *)

val mem : ('a, 'b) t -> 'a -> bool
(** [mem t k] is [true] if [k] has a binding. *)

val remove : ('a, 'b) t -> 'a -> unit
(** [remove t k] removes the binding of [k]. It does nothing if there is none. *)

val length : ('a, 'b) t -> int
(** The number of bindings. *)

val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
(** [iter f t] calls [f k v] for each binding, in insertion order. *)

val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
(** [fold f t init] is [f kN vN (... (f k1 v1 init))], with bindings in
    insertion order. *)
