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

  let pool_id = Dbcaml.start_link ~connections:10 driver |> Result.get_ok in

  (match Dbcaml.fetch_one pool_id "select * from users" with
  | Ok x -> Printf.printf "%Sn" (String.of_bytes x)
  | Error x -> print_endline (Dbcaml.Res.execution_error_to_string x));

  (match
     Dbcaml.fetch_one
       pool_id
       ~params:[Dbcaml.Param.Number 20]
       "select * from users where id > $1"
   with
  | Ok x -> Printf.printf "fetch_one response %Sn" (String.of_bytes x)
  | Error x -> print_endline (Dbcaml.Res.execution_error_to_string x));

  (*
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
  ()
