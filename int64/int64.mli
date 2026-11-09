(** Signed 64-bit integers, a small subset of [Stdlib.Int64].

    Each function has the same name and the same result as the function in
    [Stdlib.Int64], except where this file says otherwise.

    On native, with [melange-tiny.int64.native] (the default), [t] is [int64]
    and each function calls [Stdlib.Int64]. On Melange, with
    [melange-tiny.int64.js], [t] is a JavaScript [BigInt] that each function
    keeps in the signed 64-bit range. A [melange.emit] stanza must list
    [melange-tiny.int64.js] in its [libraries]. The Melange build does not load
    the [caml_int64] runtime or [Stdlib.Int64]. JavaScript engines without
    [BigInt] (before ES2020) are not supported.

    Not included: [unsigned_*] functions, [of_int32], [to_int32],
    [of_nativeint], [to_nativeint], [bits_of_float], [float_of_bits],
    [format], [hash], [seeded_hash], and the [1L] literal syntax. Make a value
    with {!of_int}, {!of_float} or {!of_string}.

    Differences from [Stdlib.Int64]:
    - [t] is abstract. It is not compatible with [int64], [Printf] ["%Ld"]
      or [Stdlib.Int64].
    - On Melange, do not give [t] to [Hashtbl.hash] or [Marshal]. Use
      {!equal} and {!compare}. Polymorphic [=] and [compare] work on both
      platforms. *)

type t

val zero : t
val one : t
val minus_one : t

val max_int : t
(** [9223372036854775807]. *)

val min_int : t
(** [-9223372036854775808]. *)

val neg : t -> t
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t

val div : t -> t -> t
(** Rounds toward zero. Raises [Division_by_zero] if the second argument is
    zero. [div min_int minus_one] is [min_int]. *)

val rem : t -> t -> t
(** The sign of the result is the sign of the first argument. Raises
    [Division_by_zero] if the second argument is zero. *)

val succ : t -> t
val pred : t -> t

val abs : t -> t
(** [abs min_int] is [min_int]. *)

val logand : t -> t -> t
val logor : t -> t -> t
val logxor : t -> t -> t
val lognot : t -> t

val shift_left : t -> int -> t
(** Difference: the shift count is [n land 63] on all platforms. The stdlib
    result is unspecified when [n < 0] or [n >= 64]. *)

val shift_right : t -> int -> t
(** Arithmetic shift. The shift count is [n land 63], as in {!shift_left}. *)

val shift_right_logical : t -> int -> t
(** Logical shift. The shift count is [n land 63], as in {!shift_left}. *)

val of_int : int -> t

val to_int : t -> int
(** Keeps the low bits that fit in [int]: 63 bits on native, 32 bits on
    Melange, as [Stdlib.Int64.to_int] does on each platform. *)

val of_float : float -> t
(** Rounds toward zero. As in the stdlib, the result is unspecified when the
    float is [nan], infinite, or outside the [t] range. On Melange, [nan] and
    infinite values give [zero], and other values outside the range wrap
    modulo 2{^64}. *)

val to_float : t -> float
(** Rounds to the nearest float. *)

val of_string : string -> t
(** Reads a decimal number, or a number with the prefix [0x], [0o], [0b] or
    [0u], with an optional [-] or [+] sign and [_] between digits. Raises
    [Failure "Int64.of_string"] if the text is not a number or does not fit in
    64 bits. *)

val of_string_opt : string -> t option
val to_string : t -> string
(** Decimal text, the same as [Printf.sprintf "%Ld"]. *)

val compare : t -> t -> int
(** Returns [-1], [0] or [1]. *)

val equal : t -> t -> bool
val min : t -> t -> t
val max : t -> t -> t
