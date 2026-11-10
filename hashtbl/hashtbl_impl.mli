(** The platform part of {!Tiny_hashtbl.Hashtbl}. Use {!Tiny_hashtbl.Hashtbl}. *)

type (!'a, !'b) t

include
  module type of Stdlib__Hashtbl
    with type ('a, 'b) t := ('a, 'b) t
     and type statistics = Stdlib.Hashtbl.statistics
