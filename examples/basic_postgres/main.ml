open Riot

open Logger.Make (struct
  let namespace = ["dbcaml"; "dbcaml_driver_postgres"]
end)

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in

  set_log_level (Some Logger.Debug);

  info (fun f -> f "Starting application");

  let driver =
    Dbcaml_driver_postgres.connection
      "postgresql://postgres:postgres@localhost:6432/postgres"
  in

  (match Dbcaml.Driver.connect driver with
  | Ok _ -> debug (fun f -> f "connected")
  | Error (`Msg e) -> error (fun f -> f "failed to start connection: %s" e)
  | Error _ -> error (fun f -> f "got a weird error"));

  ()

(*
    (* Fetch 1 row from the database *)
    (match
       Silo.fetch_one
         pool_id
         ~params:[Dbcaml.Connection.Number 1]
         "select * from users where id = $1"
     with
    | Ok x ->
      let rows = Silo.Row.row_to_type x in
      (* Iterate over each column and print it's values *)
      List.iter (fun x -> print_endline x) rows
    | Error x -> print_endline (Dbcaml.Res.execution_error_to_string x));

    (* Fetch multiple rows from the database *)
    (match
       Silo.fetch_many
         pool_id
         ~params:[Dbcaml.Connection.Number 1]
         "select * from users where id = $1"
     with
    | Ok x ->
      List.iter
        (fun x ->
          let rows = Silo.Row.row_to_type x in
          (* Iterate over each column and print it's values *)
          List.iter (fun x -> print_endline x) rows)
        x
    | Error x -> print_endline (Dbcaml.Res.execution_error_to_string x));

    (* Exec a query to the database *)
    (match
       Silo.exec
         pool_id
         ~params:[Dbcaml.Connection.Number 1]
         "select * from users where id = $1"
     with
    | Ok _ -> print_endline "Executed successfully"
    | Error x -> print_endline (Dbcaml.Res.execution_error_to_string x));

    () *)
