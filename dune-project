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
   (>= 0.0.9))
  uri)
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
   (>= 0.0.9))
  (dbcaml :version)
  bytestring
  cryptokit
  (serde-postgres :version)
  castore
  random
  uri)
 (tags
  (topics "database" "dbcaml" "dbcaml")))

(package
 (name silo)
 (synopsis "Silo")
 (description "Silo")
 (depends
  ocaml
  (alcotest :with-test)
  dune
  serde
  serde_derive
  (serde-postgres :version))
 (tags
  (topics "postgres" "ocaml")))

(package
 (name serde-postgres)
 (synopsis "serde-postgres")
 (description "serde-postgres")
 (depends
  ocaml
  (alcotest :with-test)
  dune
  (serde
   (>= 0.0.2))
  (serde_derive
   (>= 0.0.2)))
 (tags
  (topics "postgres" "serde" "ocaml")))
