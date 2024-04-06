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
 (depends
  ocaml
  dune
  (alcotest :with-test)
  (riot
   (>= 0.0.8))
  uri
  streaming)
 (tags
  (topics "database" "dbcaml")))

(package
 (name dbcaml-driver-postgres)
 (synopsis "Postgres driver for dbcaml")
 (description "Postgres driver for dbcaml")
 (depends
  ocaml
  (alcotest :with-test)
  dune
  (riot
   (>= 0.0.8))
  dbcaml
  bytestring
  cryptokit
  castore
  random
  streaming
  uri)
 (tags
  (topics "database" "dbcaml" "dbcaml")))

(package
 (name silo-postgres)
 (synopsis "Silo")
 (description "Silo")
 (depends
  ocaml
  (alcotest :with-test)
  dune
  streaming
  dbcaml)
 (tags
  (topics "postgres" "ocaml")))
