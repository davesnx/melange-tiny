(** The platform part of {!Tiny_printf.Printf}. Use {!Tiny_printf.Printf}. *)

val float_g : int -> float -> string
(** [float_g p f] is [Printf.sprintf "%.*g" p f] for [p >= 0]. *)
