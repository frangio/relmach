(lang dune 3.17)
(name relmach)

(package
 (name relmach)
 (depends
  (ocaml (>= 4.14))
  angstrom
  (dune (< 3.21))
  (qcheck-core (< 0.90))
  ppx_inline_test))
