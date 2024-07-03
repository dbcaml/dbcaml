open Alcotest

let () =
  Riot.run @@ fun () ->
  run
    "Silo"
    [
      ("Parse command complete", Parse_command_complete.suite);
      ("Postgres: Queries and deserialize data", Postgres_query.suite);
      ("Postgres: Executes Queries", Postgres_execute.suite);
    ]
