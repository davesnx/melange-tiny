(** Message digests: a drop-in replacement for [Stdlib.Digest].

    On native, this module is [Stdlib.Digest].

    On Melange, it has the same signature, results and exceptions as the
    Melange [Stdlib.Digest], with a small MD5 implementation. A bundle that
    uses only MD5 functions does not get the [Stdlib.Digest] module, the
    [caml_md5] runtime, channels or the BLAKE2 modules.

    As in the Melange [Stdlib.Digest]:
    - [channel], [file], [MD5.channel] and all BLAKE2 hash functions call the
      runtime primitives [caml_md5_chan], [caml_ml_open_descriptor_in] and
      [caml_blake2_*]. Melange does not supply them, so these functions raise
      a JavaScript [Error] unless the program defines them on [globalThis].
    - [input] and [output] use [Stdlib.really_input_string] and
      [Stdlib.output_string].
    - A string that comes from JavaScript can contain code units above 255.
      Hash functions and [to_hex] give the same result or exception for such
      strings.

    One difference on Melange: [Stdlib.Digest.substring s ofs len],
    [Stdlib.Digest.subbytes b ofs len] and the same functions in
    [Stdlib.Digest.MD5] hash the bytes from [ofs] to position [len] when
    [ofs > 0], not [len] bytes from [ofs]. This module gives the correct
    digest, the same as on native. *)

include module type of Stdlib.Digest
