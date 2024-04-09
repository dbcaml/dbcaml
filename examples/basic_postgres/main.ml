open Riot

open Logger.Make (struct
  let namespace = ["dbcaml"; "dbcaml_driver_postgres"]
end)

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in

  set_log_level (Some Logger.Debug);

  info (fun f -> f "Starting application");

  let pool_id =
    match
      Silo_postgres.start
        ~connections:4
        "postgresql://postgres:postgres@localhost:6432/postgres?sslmode=disabled"
    with
    | Ok conn -> conn
    | Error e -> failwith e
  in

  let _ =
    match
      Silo_postgres.fetch_one pool_id ~query:"select * from users limit 10"
    with
    | Ok x ->
      let _ = Silo_postgres.to_type x in
      ()
    | Error x -> print_endline (Dbcaml.Res.execution_error_to_string x)
  in

  ()
