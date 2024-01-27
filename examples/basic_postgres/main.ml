open Dbcaml_driver_postgres
open Dbcaml
open Riot

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in

  Logger.set_log_level (Some Logger.Debug);

  Logger.info (fun f -> f "Starting application");

  let connection =
    Dbcaml.start_link
      (Dbcaml_driver_postgres.start_link
         "postgresql://postgres:postgres@127.0.0.1:6432/postgres?sslmode=disable")
      ~max_connections:10
  in
  let data =
    Dbcaml.query connection "SELECT * FROM users" |> dbcaml.map_to Users.t
  in

  ()
