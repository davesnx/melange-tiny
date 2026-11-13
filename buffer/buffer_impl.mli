(** The platform part of {!Tiny_buffer.Buffer}. Use {!Tiny_buffer.Buffer}. *)

type t

include module type of Stdlib__Buffer with type t := t
