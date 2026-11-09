(** The platform part of {!Tiny_random.Random}. Use {!Tiny_random.Random}. *)

val seed : unit -> int
(** A seed from the platform's entropy source. *)
