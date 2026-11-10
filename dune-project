(lang dune 3.16)

(using melange 0.1)

(name melange-tiny)

(generate_opam_files true)

(source
 (github davesnx/melange-tiny))

(authors "David Sancho Moreno")

(maintainers "David Sancho Moreno")

(license MIT)

(package
 (name melange-tiny)
 (synopsis
  "Small drop-in replacements of OCaml stdlib modules for Melange, to keep JavaScript bundles small")
 (description
  "Drop-in replacements of Printf, Hashtbl, Random and Digest with the stdlib signature and behaviour. They compile to small JavaScript with Melange and are the stdlib modules on native OCaml.")
 (depends
  (ocaml
   (>= 5.4))
  (melange
   (>= 7.0.1-54))))
