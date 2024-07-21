open Alcotest

let () = run "Sqlite drivers" [("Create connection object", Connection.suite)]
