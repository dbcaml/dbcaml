open Dbcaml_driver_postgres
open Riot

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in

  Logger.set_log_level (Some Logger.Debug);

  Logger.info (fun f -> f "Starting application");

  let pool =
    Postgres.connect
      ~max_connections:10
      "host=localhost port=6437 user=postgres password=mysecretpassword dbname=postgres"
  in

  let _ = pool in

  ()
