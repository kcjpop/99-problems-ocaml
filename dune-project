(lang dune 3.8)

(name hello)

(generate_opam_files true)

(source
 (github kcjpop/99-problems))

(authors "An Cao <kcj.pop.com@gmail.com>")

(maintainers "An Cao <kcj.pop.com@gmail.com>")

(license MPL-2.0)

(package
 (name hello)
 (description "99 problems in OCaml")
 (depends
  ocaml
  dune
  ppx_inline_test
  (alcotest :with-test))
 (tags ("99 problems")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
