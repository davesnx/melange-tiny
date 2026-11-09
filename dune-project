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
  "Small replacements of OCaml stdlib modules for Melange, to keep JavaScript bundles small")
 (description
  "Printf, Hashtbl, Random and Digest (MD5) subsets that compile to small JavaScript with Melange and run on native OCaml too.")
 (depends
  (ocaml
   (>= 5.4))
  (melange
   (>= 7.0.1-54))))
