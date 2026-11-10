(** [Stdlib.Hashtbl] with a small JavaScript bundle on Melange.

    The signature is the signature of [Stdlib.Hashtbl] of the platform, and
    the behaviour is the same. See the [Stdlib.Hashtbl] documentation.

    Two implementations share this interface:
    - [melange-tiny.hashtbl.native] (native, the default) is [Stdlib.Hashtbl].
    - [melange-tiny.hashtbl.js] (Melange) has the algorithm of the Melange
      [Stdlib.Hashtbl]: the same hash function, bucket layout, resize rule,
      iteration order and exceptions. It does not use [Random] and [Domain].
      With [~random:true] or after {!randomize}, the seed comes from
      [Math.random]. A [melange.emit] stanza must list it in its
      [libraries]. *)

type (!'a, !'b) t
(** A table from keys of type ['a] to values of type ['b]. *)

include
  module type of Stdlib__Hashtbl
    with type ('a, 'b) t := ('a, 'b) t
     and type statistics = Stdlib.Hashtbl.statistics
