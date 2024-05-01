let test_successful_query_no_params () =
  let connection =
    Dbcaml_driver_postgres.connection
      "postgresql://postgres:postgres@localhost:6432/postgres?sslmode=disabled"
  in

  let conn =
    match connection with
    | Driver { driver = (module DriverModule); config } ->
      (match DriverModule.connect config with
      | Ok e -> Ok e
      | Error (`Msg e) ->
        Alcotest.fail
          (Printf.sprintf "should be able to start a connection: %S" e)
      | Error _ -> Alcotest.fail "Should be able to start a connection")
  in
  let conn = Result.get_ok conn in

  let result =
    match
      Dbcaml.Connection.query
        ~conn
        ~params:[]
        ~query:"select * from users limit 2"
        ~row_limit:0
    with
    | Ok s -> Ok (Bytes.to_string s)
    | Error e -> Alcotest.fail (Dbcaml.Res.execution_error_to_string e)
  in
  let result = Result.get_ok result in
  let first_char = result.[0] in

  Alcotest.(check char) "Does return a successful response" 'T' first_char

let test_successful_query_with_params () =
  let connection =
    Dbcaml_driver_postgres.connection
      "postgresql://postgres:postgres@localhost:6432/postgres?sslmode=disabled"
  in

  let conn =
    match connection with
    | Driver { driver = (module DriverModule); config } ->
      (match DriverModule.connect config with
      | Ok e -> Ok e
      | Error (`Msg e) ->
        Alcotest.fail
          (Printf.sprintf "should be able to start a connection: %S" e)
      | Error _ -> Alcotest.fail "Should be able to start a connection")
  in
  let conn = Result.get_ok conn in

  let result =
    match
      Dbcaml.Connection.query
        ~conn
        ~params:[Dbcaml.Param.Number 10]
        ~query:"select * from users where id > $1 limit 2"
        ~row_limit:0
    with
    | Ok s -> Ok (Bytes.to_string s)
    | Error e -> Alcotest.fail (Dbcaml.Res.execution_error_to_string e)
  in
  let result = Result.get_ok result in
  let first_char = result.[0] in

  Alcotest.(check char) "Does return a successful response" 'T' first_char

let test_unsuccessful_query () =
  let connection =
    Dbcaml_driver_postgres.connection
      "postgresql://postgres:postgres@localhost:6432/postgres?sslmode=disabled"
  in

  let conn =
    match connection with
    | Driver { driver = (module DriverModule); config } ->
      (match DriverModule.connect config with
      | Ok e -> Ok e
      | Error (`Msg e) ->
        Alcotest.fail
          (Printf.sprintf "should be able to start a connection: %S" e)
      | Error _ -> Alcotest.fail "Should be able to start a connection")
  in
  let conn = Result.get_ok conn in

  match
    Dbcaml.Connection.query
      ~conn
      ~params:[Dbcaml.Param.Number 10]
      ~query:"select * from users where i_dont_exist > $1 limit 2"
      ~row_limit:0
  with
  | Ok r ->
    Printf.printf "%S" (Bytes.to_string r);
    Alcotest.fail "We should error here"
  | Error _ -> ()
