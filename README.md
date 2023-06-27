[![Ocaml workflow](https://github.com/kcjpop/99-problems-ocaml/actions/workflows/ml.yml/badge.svg)](https://github.com/kcjpop/99-problems-ocaml/actions/workflows/ml.yml)

## 99 problems in OCaml

From https://ocaml.org/problems. Tests are inline with `ppx_inline_test`.

## To run

Need to have OCaml and opam installed, then:

```bash
# To install dependencies
$ opam install --deps-only --with-test .
$ dune runtest
```
