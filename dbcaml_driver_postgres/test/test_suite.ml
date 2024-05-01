let () =
  Riot.run @@ fun () ->
  Alcotest.run
    "Postgres Driver tests"
    [
      ( "Database Queries",
        [
          ("Successful query no params", `Quick, Queries.test_successful_query);
          ( "Successful query with params",
            `Quick,
            Queries.test_successful_query_with_params );
        ] );
    ]
