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
      Silo.config
        ~connections:5
        ~driver:(module Dbcaml_driver_postgres)
        ~connection_string:
          "postgresql://postgres:postgres@localhost:6432/postgres?sslmode=disabled"
    in

    Silo.connect ~config
  in

  (* Fetch the user and return the user to a variable *)
  let* rows_affected =
    Silo.execute
      db
      ~params:
        [
          Silo.Params.String "Emil";
          Silo.Params.Bool true;
          Silo.Params.String "Danza";
          Silo.Params.Number 1;
          Silo.Params.Number 1;
          Silo.Params.Float 1.1;
          Silo.Params.StringArray ["Danza"];
        ]
      ~query:
        "insert into users (name, some_bool, pet_name, some_int64, some_int32, some_float, pets) values ($1, $2, $3, $4, $5, $6, $7)"
  in

  let _ = rows_affected in

  (* Fetch the user and return the user to a variable *)
  let* rows_affected =
    Silo.execute
      db
      ~params:[Silo.Params.String "Emil"; Silo.Params.String "Lowa"]
      ~query:"update users set pet_name = $2 where name = $1"
  in

  let _ = rows_affected in

  (* Fetch the user and return the user to a variable *)
  let* rows_affected =
    Silo.execute
      db
      ~params:[Silo.Params.String "Emil"]
      ~query:"delete from users where name = $1"
  in

  let _ = rows_affected in

  Ok 1
