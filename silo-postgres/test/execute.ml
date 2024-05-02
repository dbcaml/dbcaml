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

let test_execute_successfully () =
  let db =
    match
      let config =
        Silo_postgres.config
          ~connections:1
          ~connection_string:
            "postgresql://postgres:postgres@localhost:6432/postgres?sslmode=disabled"
      in

      match Silo_postgres.connect ~config with
      | Ok c -> Ok c
      | Error (`Msg e) -> Error e
    with
    | Ok c -> c
    | Error e -> Alcotest.fail e
  in

  (* Fetch the user and return the user to a variable *)
  let rows_affected =
    match
      Silo_postgres.execute
        db
        ~params:
          [
            Silo_postgres.Params.String "hello-world";
            Silo_postgres.Params.Bool true;
            Silo_postgres.Params.String "Danza";
            Silo_postgres.Params.Number 1;
            Silo_postgres.Params.Number 1;
            Silo_postgres.Params.Float 1.1;
            Silo_postgres.Params.StringArray ["Danza"];
          ]
        ~query:
          "insert into users (name, some_bool, pet_name, some_int64, some_int32, some_float, pets) values ($1, $2, $3, $4, $5, $6, $7)"
    with
    | Ok r -> r
    | Error e -> Alcotest.fail e
  in

  if rows_affected == 0 then
    Alcotest.fail "insert query should have returned at least 1 row affected";

  (* Fetch the user and return the user to a variable *)
  let rows_affected =
    match
      Silo_postgres.execute
        db
        ~params:
          [
            Silo_postgres.Params.String "hello-world-2";
            Silo_postgres.Params.String "hello-world";
          ]
        ~query:"update users set name = $1 where name = $2"
    with
    | Ok r -> r
    | Error e -> Alcotest.fail e
  in

  if rows_affected == 0 then
    Alcotest.fail "update query should have returned at least 1 row affected";
  (* Fetch the user and return the user to a variable *)
  let rows_affected =
    match
      Silo_postgres.execute
        db
        ~params:[Silo_postgres.Params.String "hello-world-2"]
        ~query:"delete from users where name = $1"
    with
    | Ok r -> r
    | Error e -> Alcotest.fail e
  in

  if rows_affected == 0 then
    Alcotest.fail "Delete query should have returned at least 1 row affected"

let suite =
  [test_case "Execute insert successfully" `Quick test_execute_successfully]
