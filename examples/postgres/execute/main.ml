open Riot

open Logger.Make (struct
  let namespace = ["examples"; "basic_postgres"]
end)

let ( let* ) = Result.bind

let () =
  Riot.run_with_status ~on_error:(fun x -> failwith x) @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in
  set_log_level (Some Logger.Debug);
  info (fun f -> f "Starting application");

  (* Start the database connection pool *)
  let* db =
    let config =
      Silo_postgres.config
        ~connections:5
        ~connection_string:
          "postgresql://postgres:postgres@localhost:6432/postgres?sslmode=disable"
    in

    match Silo_postgres.connect ~config with
    | Ok c -> Ok c
    | Error (`Msg e) -> Error e
  in

  (* Fetch the user and return the user to a variable *)
  let* rows_affected =
    Silo_postgres.execute
      db
      ~params:
        [
          Silo_postgres.Params.String "Emil";
          Silo_postgres.Params.Bool true;
          Silo_postgres.Params.String "Danza";
          Silo_postgres.Params.Number 1;
          Silo_postgres.Params.Number 1;
          Silo_postgres.Params.Float 1.1;
          Silo_postgres.Params.StringArray ["Danza"];
        ]
      ~query:
        "insert into users (name, some_bool, pet_name, some_int64, some_int32, some_float, pets) values ($1, $2, $3, $4, $5, $6, $7)"
  in

  let _ = rows_affected in

  (* Fetch the user and return the user to a variable *)
  let* rows_affected =
    Silo_postgres.execute
      db
      ~params:
        [Silo_postgres.Params.String "Emil"; Silo_postgres.Params.String "Lowa"]
      ~query:"update users set pet_name = $2 where name = $1"
  in

  let _ = rows_affected in

  (* Fetch the user and return the user to a variable *)
  let* rows_affected =
    Silo_postgres.execute
      db
      ~params:[Silo_postgres.Params.String "Emil"]
      ~query:"delete from users where name = $1"
  in

  let _ = rows_affected in

  Ok 1
