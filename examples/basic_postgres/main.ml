open Riot

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in

  Logger.set_log_level (Some Logger.Debug);

  Logger.info (fun f -> f "Starting application");

  let driver =
    Dbcaml_driver_postgres.connection
      "postgresql://postgres:mysecretpassword@127.0.0.1:6432/development?sslmode=disable"
  in

  let conn = Dbcaml.Dbcaml.start_link driver in

  let _ = conn in

  ()
