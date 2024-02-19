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


DBCaml is a async database toolkit built on <a href="https://github.com/riot-ml/riot">Riot</a>, an actor-model multi-core scheduler for OCaml 5. DBCaml is inspired by [Elixirs](https://github.com/elixir-ecto/ecto) where the developer can spin up a connection manager and connections the manager takes cares of. 

```ocaml

let driver =
    Dbcaml_driver_postgres.connection
      "postgresql://postgres:mysecretpassword@localhost:6432/development"
  in

  let pool_id = Dbcaml.start_link ~connections:10 driver |> Result.get_ok in

  (* Fetch 1 row from the database *)
  (match
     Dbcaml.fetch_one
       pool_id
       ~params:[Dbcaml.Param.String "1"]
       "select * from users where id = $1"
   with
  | Ok x ->
    let rows = Dbcaml.Row.row_to_type x in
    (* Iterate over each column and print it's values *)
    List.iter (fun x -> print_endline x) rows
  | Error x -> print_endline (Dbcaml.Res.execution_error_to_string x));



```
DBCaml aims to offer:

* **Database pooling**. Built with using Riots lightweight process to spin up a connection pool.

* **Database Agnostic**. Support for Postgres, and more to come(MySQL, MariaDB, SQLite)

* **Built in security**. With built in security allows users to focus on writing queries and don't be afraid of security breaches.

* **Cross Platform**. DBCaml compiles anywhere

* **Not an ORM**. DBCaml is not an orm, it simple handle the boring stuff you don't want to deal with and allow you to have full insight on what's going on.

## Quick Start

```
opam pin dbcaml.0.0.1 git+https://github.com/dbcaml/dbcaml
```

After that, you can use any of the [examples](./examples) as a base for your app, and run them:

```
dune exec ./my_app.exe
```
# Important
DBCaml is in heavily development, the content in this repo will change
