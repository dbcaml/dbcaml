(lang dune 3.12)

(name dbcaml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Emil Privér <emil@priver.dev>")

(maintainers "Emil Privér <emil@priver.dev>")

(license MIT)

(documentation https://github.com/emilpriver/dbcaml)

(package
 (name dbcaml)
 (synopsis "Database toolkit for OCaml")
 (description "Database toolkit for OCaml")
 (depends ocaml dune)
 (tags
  (topics "daatabase" "ocaml")
  ))
