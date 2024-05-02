open Alcotest

let () =
  Riot.run @@ fun () ->
  run
    "Silo Postgres"
    [
      ("Parse command complete", Parse_command_complete.suite);
      ("Queries and deserialize data", Query.suite);
      ("Executes Queries", Execute.suite);
    ]
