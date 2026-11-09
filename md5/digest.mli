(** MD5 digests of strings: a small subset of [Stdlib.Digest].

    One OCaml implementation is used on native and on Melange. On Melange it
    replaces [Stdlib.Digest] and the [caml_md5] runtime, so a bundle that uses
    only this module does not get [Digest], [Bytes] channels or BLAKE2.

    Each function has the same name, result and exceptions as the function
    with the same name in [Stdlib.Digest] (MD5, 16 bytes).

    Not included: [bytes], [subbytes], [channel], [file], [output], [input],
    the [S] module type and the [BLAKE128], [BLAKE256], [BLAKE512] and [MD5]
    submodules. A browser runtime has no channels or files, and it works on
    strings, not bytes.

    Difference on Melange: an OCaml [int] is 32 bits there, so the input
    must be shorter than 2{^28} bytes (256 MB). On native the limit is the
    string size limit. *)

type t = string
(** A digest: a string of 16 bytes. *)

val compare : t -> t -> int
(** Same as [String.compare]. *)

val equal : t -> t -> bool
(** Same as [String.equal]. *)

val string : string -> t
(** [string s] is the MD5 digest of [s]. *)

val substring : string -> int -> int -> t
(** [substring s ofs len] is the MD5 digest of the [len] bytes of [s] that
    start at position [ofs]. On Melange, [Stdlib.Digest.substring] with
    [ofs > 0] hashes the bytes from [ofs] to [len], not [len] bytes from
    [ofs]. This function gives the correct digest on both platforms.
    @raise Invalid_argument ["Digest.substring"] if [ofs] and [len] do not
    give a valid substring of [s]. *)

val to_hex : t -> string
(** [to_hex d] is [d] as 32 lowercase hexadecimal characters.
    @raise Invalid_argument ["Digest.to_hex"] if [d] is not 16 bytes long. *)

val of_hex : string -> t
(** [of_hex s] is the digest that [s] writes in hexadecimal. Upper and lower
    case letters are accepted.
    @raise Invalid_argument ["Digest.from_hex"] if [s] is not 32 characters
    long.
    @raise Invalid_argument ["Digest.of_hex"] if [s] has a character that is
    not a hexadecimal digit. *)

val from_hex : string -> t
(** Same as [of_hex]. *)
