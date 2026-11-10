(** The platform part of {!Tiny_printf.Printf}. Use {!Tiny_printf.Printf}. *)

include module type of struct
  include Stdlib.Printf
end
