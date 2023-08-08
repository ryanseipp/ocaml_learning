(lang dune 3.10)

(name ocaml_learning)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name ocaml_learning)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune (ppx_inline_test :with-test))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
