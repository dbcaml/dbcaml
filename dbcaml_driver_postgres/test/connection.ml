let test_connection_should_fail () =
  let connection =
    Dbcaml_driver_postgres.connection
      "postgresql://postgres-i-dont-exist:postgres@localhost:1337/postgres?sslmode=disabled"
  in

  match connection with
  | Driver { driver = (module DriverModule); config } ->
    (match DriverModule.connect config with
    | Ok _ ->
      Alcotest.fail
        "should not be able to start connection as the port is wrong"
    | Error _ -> ())
