(lang dune 3.16)

(name aoc)

(generate_opam_files true)

(source
 (github max-legrand/aoc))

(authors "Maxwell Legrand")

(maintainers "Maxwell Legrand")

(license MIT)

(wrapped_executables false)

(package
 (name aoc)
 (synopsis "Advent of Code solutions")
 (description "Advent of Code solutions in OCaml")
 (depends
   ocaml
   dune
   stdio
   base
   core
   re
   lwt
   core_unix
   utop
   )
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
