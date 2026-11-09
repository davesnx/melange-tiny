(** A small subset of [Stdlib.Random] for native OCaml and Melange.

    The functions have the same names, types, ranges and exceptions as the
    stdlib ones. The generator is xorshift128 on 32-bit words, so it needs no
    [Int64] and no [Digest] on Melange.

    Differences from [Stdlib.Random]:
    - The numbers are not the same as the stdlib numbers for the same seed.
      The stdlib also gives different numbers on native (LXM) and on Melange
      (lagged Fibonacci). This module gives the same numbers on native and on
      Melange for a seed in the 32-bit range.
    - There is one global state. It is not per domain and not safe to use from
      two domains at the same time.
    - Not included: [full_init], [full_int], [int_in_range], [int32], [int64],
      [nativeint] and their range forms, [bits32], [bits64], [nativebits],
      [split], [get_state], [set_state] and [State].
    - [init] uses only the low 32 bits of the seed, after a fold of bits 32 to
      62 into them on native.
    - The generator is not suitable for cryptography, as in the stdlib.

    The library has one virtual module, [Random_impl], that gives the seed
    for [self_init]. [melange-tiny.random.native] (native, the default) reads it
    from the [caml_sys_random_seed] runtime primitive, as the stdlib does.
    [melange-tiny.random.js] (Melange) reads it from [Math.random]. A
    [melange.emit] stanza must list it in its [libraries]. *)

val init : int -> unit
(** Initialize the generator with a seed. The same seed gives the same
    numbers. *)

val self_init : unit -> unit
(** Initialize the generator with a seed from the platform. Before a call to
    [init] or [self_init], the generator has a fixed seed, as in the stdlib. *)

val bits : unit -> int
(** 30 random bits, in [0, 2{^30} - 1]. *)

val int : int -> int
(** [int bound] is a random integer in [0, bound - 1].
    @raise Invalid_argument
      ["Random.int"] if [bound <= 0] or [bound > 2{^30} - 1]. *)

val float : float -> float
(** [float bound] is a random float between [0] and [bound]. A negative
    [bound] gives a result that is negative or zero. The result is
    [r *. bound], where [r] has 53 random bits and is in [\[0, 1)]. *)

val bool : unit -> bool
(** A random boolean. *)
