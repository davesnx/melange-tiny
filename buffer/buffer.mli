(** Extensible buffers: a drop-in replacement for [Stdlib.Buffer].

    On native, this module is [Stdlib.Buffer].

    On Melange, it has the same signature, results and exceptions as the
    Melange [Stdlib.Buffer]. The buffer is a JavaScript string that grows by
    concatenation. A bundle that does not call [to_bytes], [add_bytes],
    [add_subbytes], [blit], [add_channel] or [output_buffer] does not get the
    [Stdlib.Buffer] and [Stdlib.Bytes] modules or the bytes runtime.

    As in the Melange [Stdlib.Buffer]:
    - [add_channel] and [output_buffer] call the runtime primitives
      [caml_ml_input] and [caml_ml_output_bytes]. Melange does not supply
      them, so these functions raise a JavaScript [Error] unless the program
      defines them on [globalThis].
    - A string that comes from JavaScript can contain code units above 255.
      The buffer keeps them.
    - [add_int8 b x] and [add_uint8 b x] add the code unit [x] when
      [0 <= x <= 0xffff], not [x land 0xff].

    The type [t] is not [Stdlib.Buffer.t]. Functions of other modules that
    take a [Stdlib.Buffer.t], such as [Printf.bprintf], do not accept it.

    Differences on Melange:
    - [add_int8 b x] and [add_uint8 b x] with [x < 0] or [x > 0xffff]:
      [contents] is the same, but [nth], [to_seq], [to_seqi], [to_bytes] and
      [blit] give the code unit [x land 0xffff]. [Stdlib.Buffer] gives [x],
      which is not a valid [char].
    - [add_utf_8_uchar], [add_utf_16le_uchar] and [add_utf_16be_uchar] with a
      value from [Uchar.unsafe_of_int] that is below 0 or above [0x10FFFF]
      do not raise [Assert_failure]. *)

type t
(** The abstract type of buffers. *)

include module type of Stdlib__Buffer with type t := t
