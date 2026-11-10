(** [Stdlib.Random] for native OCaml and Melange.

    The signature is the signature of [Stdlib.Random] on each platform. On
    native OCaml it has more functions than on Melange, for example
    [int_in_range], [nativeint] and [split]. [State.t] is
    [Stdlib.Random.State.t] on both platforms, so a state from this module
    and a state from [Stdlib.Random] are interchangeable.

    [melange-tiny.random.native] (native, the default) is [Stdlib.Random].

    [melange-tiny.random.js] (Melange) is a small implementation of the
    Melange [Stdlib.Random]. For the same seed it gives the same numbers and
    raises the same exceptions. It does not use [Digest] or [Printf]. Only
    [int64] and [bits64] use the 64-bit integer runtime, so a program that
    does not call them does not bundle it. A [melange.emit] stanza must list it
    in its [libraries].

    The numbers are different on native OCaml and on Melange, as in the
    stdlib. *)

include module type of Stdlib.Random
