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
        ~connections:10
        "postgresql://postgres:postgres@localhost:6432/postgres?sslmode=disabled"
    with
    | Ok conn -> conn
    | Error e -> failwith e
  in

  (match Silo_postgres.fetch_one pool_id ~query:"select * from users" with
  | Ok x -> Printf.printf "%Sn" (String.of_bytes x)
  | Error x -> print_endline (Dbcaml.Res.execution_error_to_string x));

  ()
