(lang dune 3.6)

(using menhir 2.1)

(name lambdo)

(generate_opam_files true)

(source
 (github YassineHaouzane/Lambdo))

(authors "Yassine Haouzane")

(package
 (name lambdo)
 (description "Small lambda calculus interpreter")
 (depends ocaml dune menhir ppx_deriving.show cmdliner))
