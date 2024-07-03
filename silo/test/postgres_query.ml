open Alcotest

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

let test_query_sucessfully () =
  let db =
    match
      let config =
        Silo.config
          ~connections:1
          ~driver:(module Dbcaml_driver_postgres)
          ~connection_string:
            "postgresql://postgres:postgres@localhost:6432/postgres?sslmode=disable"
      in

      Silo.connect ~config
    with
    | Ok c -> c
    | Error e -> Alcotest.fail e
  in

  (* Fetch the user and return the user to a variable *)
  let fetched_user =
    match
      Silo.query
        db
        ~params:[Silo.string "Bob"]
        ~query:
          "select name, id, some_bool, pet_name, some_int64, some_int32, some_float, pets, pets as pets_array from users where name = $1 limit 1"
        ~deserializer:deserialize_user
    with
    | Ok r -> Option.get r
    | Error e -> Alcotest.fail e
  in

  Alcotest.(check string) "Does handle query" "Bob" fetched_user.name

let test_query_no_rows () =
  let db =
    match
      let config =
        Silo.config
          ~connections:1
          ~driver:(module Dbcaml_driver_postgres)
          ~connection_string:
            "postgresql://postgres:postgres@localhost:6432/postgres?sslmode=disable"
      in

      match Silo.connect ~config with
      | Ok c -> Ok c
      | Error e -> Error e
    with
    | Ok c -> c
    | Error e -> Alcotest.fail e
  in

  (* Fetch the user and return the user to a variable *)
  match
    Silo.query
      db
      ~params:[Silo.string "i_dont_exist"]
      ~query:
        "select name, id, some_bool, pet_name, some_int64, some_int32, some_float, pets, pets as pets_array from users where name = $1 limit 1"
      ~deserializer:deserialize_user
  with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "Shouldn't get any rows back"
  | Error e -> Alcotest.fail e

let test_query_unsucessfully () =
  let db =
    match
      let config =
        Silo.config
          ~connections:1
          ~driver:(module Dbcaml_driver_postgres)
          ~connection_string:
            "postgresql://postgres:postgres@localhost:6432/postgres?sslmode=disable"
      in

      match Silo.connect ~config with
      | Ok c -> Ok c
      | Error e -> Error e
    with
    | Ok c -> c
    | Error e -> Alcotest.fail e
  in

  (* Fetch the user and return the user to a variable *)
  match
    Silo.query
      db
      ~params:[Silo.string "Alice"]
      ~query:
        "select name, id, some_bool, pet_name, i_dont_exist, some_int32, some_float, pets, pets as pets_array from users where name = $1 limit 1"
      ~deserializer:deserialize_user
  with
  | Ok _ -> Alcotest.fail "Should have return a error"
  | Error _ -> ()

let suite =
  [
    test_case "Queries and deserialize data" `Quick test_query_sucessfully;
    test_case "Queries no rows" `Quick test_query_no_rows;
    test_case "Handles error from database" `Quick test_query_unsucessfully;
  ]
