(lang dune 3.12)

(name dbcaml)

(generate_opam_files true)

(source
 (github emilpriver/dbcaml))

(authors "Emil Priv\195\169r <emil@priver.dev>")

(maintainers "Emil Priv\195\169r <emil@priver.dev>")

(license MIT)

(documentation https://github.com/emilpriver/dbcaml)

(package
 (name dbcaml)
 (synopsis "Database toolkit for OCaml")
 (description "Database toolkit for OCaml")
 (depends ocaml dune)
 (tags
  (topics "database" "ocaml")))

(package
 (name dbcaml_driver_postgres)
 (synopsis "Postgres driver for dbcaml")
 (description "Postgres driver for dbcaml")
 (depends ocaml dune)
 (tags
  (topics "database" "ocaml" "dbcaml")))
