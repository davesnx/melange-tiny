(** [Stdlib.Printf] with a small JavaScript bundle on Melange.

    This module has the signature of [Stdlib.Printf], with the same types.
    Format strings are the usual OCaml format literals, for example
    [Printf.sprintf "%s at %d" msg i]. See the [Stdlib.Printf] documentation
    for each function.

    On native OCaml ([melange-tiny.printf.native], the default
    implementation) this module is [Stdlib.Printf].

    On Melange ([melange-tiny.printf.js]) the format is read by a small
    interpreter. The output text, the exceptions, the order of the calls to
    [%a] and [%t] printers and the writes to channels are the same as with the
    Melange [Stdlib.Printf]. The bundle does not get [CamlinternalFormat],
    and [sprintf] does not get [Buffer] or the channel code. A
    [melange.emit] stanza must list [melange-tiny.printf.js] in its
    [libraries]. *)

include module type of struct
  include Stdlib.Printf
end
