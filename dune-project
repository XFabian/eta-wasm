(lang dune 3.16)

(using menhir 3.0)
(name wetac)

(generate_opam_files true)

(source
 (github xaver-fabian/wetac))

(authors "Xaver Fabian")

(maintainers "Xaver Fabian <xaver.fabian@gmail.com>")

(license MIT)

(package
 (name wetac)
 (synopsis "A compiler for the Eta programming language targeting WebAssembly")
 (description
  "Wetac compiles Eta, a small imperative language with integers, booleans, arrays, records and multiple return values, to WebAssembly. It emits WAT, which can be assembled with wat2wasm. Includes a CFG-based optimization framework with dominators, dataflow analyses and constant propagation.")
 (depends
  ocaml
  dune
  menhir
  cmdliner
  sexplib
  ppx_deriving
  ppx_sexp_conv
  expect_test_helpers_core
  ppx_expect)
 (tags
  (compiler webassembly wasm eta ocaml)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
