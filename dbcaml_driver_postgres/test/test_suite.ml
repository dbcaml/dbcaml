(* This tests  equires*a running postgres database running
   If you run this tests locally is the easiest way to start the docker compose
*)

let () =
  Riot.run @@ fun () ->
  Alcotest.run
    "Postgres Driver tests"
    [
      ( "Successful query no params",
        [("", `Quick, Queries.test_successful_query_no_params)] );
      ( "Successful query with params",
        [("", `Quick, Queries.test_successful_query_with_params)] );
      ( "Handles error from postgres",
        [("", `Quick, Queries.test_unsuccessful_query)] );
      ( "Does error on connection error",
        [("", `Quick, Connection.test_connection_should_fail)] );
    ]
