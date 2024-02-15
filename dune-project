(lang dune 3.12)

(name dbcaml)

(generate_opam_files true)

(source
 (github emilpriver/dbcaml))

(authors "Emil Priver <emil@priver.dev>")

(maintainers "Emil Priver <emil@priver.dev>")

(license MIT)

(documentation https://github.com/emilpriver/dbcaml)

(package
 (name dbcaml)
 (synopsis "Database toolkit for OCaml")
 (description "Database toolkit for OCaml")
 (depends ocaml dune riot uri)
 (tags
  (topics "database" "dbcaml")))

(package
 (name dbcaml_driver_postgres)
 (synopsis "Postgres driver for dbcaml")
 (description "Postgres driver for dbcaml")
 (depends ocaml dune riot dbcaml postgresql uri)
 (tags
  (topics "database" "dbcaml" "dbcaml")))

(package
 (name querycaml)
 (synopsis "Query composer for DBcaml")
 (description "Query composer for DBcaml")
 (depends ocaml dune dbcaml)
 (tags
  (topics "database" "dbcaml")))
