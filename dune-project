(lang dune 3.16)

(using menhir 3.0)
(name wetac)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name wetac)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune cmdliner sexplib expect_test_helpers_core ppx_expect ppx_sexp_conv)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
