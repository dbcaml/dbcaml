<h1 align="center">
  <img alt="dbcaml logo" src="https://raw.githubusercontent.com/dbcaml/dbcaml/main/images/logo.png" width="300"/>
</h1>

<p align="center">
  A database toolkit built on <a href="https://github.com/riot-ml/riot">Riot</a>
</p>

<p align="center">
  <a href="#quick-start">Quick Start</a> |
  <a href="https://dbca.ml">Documentation</a> |
  <a href="https://github.com/dbcaml/dbcaml/tree/main/examples">Examples</a> |
  &nbsp;&nbsp;
</p>


DBCaml is an async database toolkit built on <a href="https://github.com/riot-ml/riot">Riot</a>, an actor-model multi-core scheduler for OCaml 5. DBCaml is inspired by [Elixirs](https://github.com/elixir-ecto/ecto) where the developer can spin up a connection manager and connections the manager takes cares of. 

** Note: DBCaml is currently in development and is not ready for production. Only for testing purposes **

```ocaml


type user = {
  name: string;
  id: int;
  some_int64: int64;
  some_int32: int32;
  some_float: float;
  some_bool: bool;
  pet_name: string option;
  pets: string list;
  pets_array: string array;
}
[@@deriving deserialize]

type users = user list [@@deriving deserialize]

(* Start the database connection pool *)
let* db =
  let config =
    Silo_postgres.config
      ~connections:5
      ~connection_string:
        "postgresql://postgres:postgres@localhost:6432/postgres?sslmode=disable"
  in

  match Silo_postgres.connect ~config with
  | Ok c -> Ok c
  | Error (`Msg e) -> Error e
in

(* Fetch the user and return the user to a variable *)
let* fetched_users =
  Silo_postgres.query
    db
    ~query:
      "select name, id, some_bool, pet_name, some_int64, some_int32, some_float, pets, pets as pets_array from users limit 2"
    ~deserializer:deserialize_users
in

List.iter
  (fun x ->
    Printf.printf
      "Fetching user with id %d:\nName: %s\nSome float: %f\nSome int64: %d\nSome int32: %d\n%s\n Some bool: %b\nPets: %s\nPets array: %s\n\n"
      x.id
      x.name
      x.some_float
      (Int64.to_int x.some_int64)
      (Int32.to_int x.some_int32)
      (match x.pet_name with
      | Some pn -> Printf.sprintf "Pet name: %S" pn
      | None -> "No pet")
      x.some_bool
      (String.concat ", " x.pets)
      (String.concat ", " (Array.to_list x.pets_array)))
  (Option.get fetched_users);
```

DBCaml aims to offer:

* **Database pooling**. Built using Riots lightweight process to spin up a connection pool.

* **Database Agnostic**. Support for Postgres, and more to come (MySQL, MariaDB, SQLite).

* **Built in security**. With built-in security it allows users to focus on writing queries, without being afraid of security breaches.

* **Cross Platform**. DBCaml compiles anywhere!

* **Not an ORM**. DBCaml is not an orm, it simply handles the boring stuff you don't want to deal with and allows you to have full insight on what's going on.

## Quick Start

```
opam pin dbcaml.0.0.2 git+https://github.com/dbcaml/dbcaml
```

After that, you can use any of the [examples](./examples) as a base for your app, and run them:
```
dune exec X
```
# Important
DBCaml is heavily in development, the content in this repo will change. It's not production ready and will probably have bugs
