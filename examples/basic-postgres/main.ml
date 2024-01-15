open Dbcaml
open Riot

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in

  Logger.set_log_level (Some Logger.Debug);

  Logger.info (fun f -> f "Starting application");

  let _ =
    PgPool.connect
      "host=localhost port=6437 user=postgres password=mysecretpassword dbname=development sslmode=disable"
  in

  ()
