open Riot

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in

  Logger.set_log_level (Some Logger.Debug);

  Logger.info (fun f -> f "Starting application");

  let driver =
    Dbcaml_driver_postgres.connection
      "postgresql://postgres:mysecretpassword@localhost:6432/development"
  in

  let conn = Dbcaml.Dbcaml.start_link driver |> Result.get_ok in

  print_endline "Sending 1 query to the database...";
  (match
     Dbcaml.Dbcaml.fetch_one
       conn
       ~params:[String "1"; Array [String "2"; String "3"]]
       "select * from users where id = $1"
   with
  | Ok x ->
    let rows = Dbcaml.Row.row_to_type x in
    (* Iterate over each column and print it's values *)
    List.iter (fun x -> print_endline x) rows
  | Error x ->
    print_endline (Dbcaml.ErrorMessages.execution_error_to_string x);
    failwith "");

  print_newline ();
  print_newline ();
  print_endline "Sending a query that will fail...";
  print_newline ();

  (match
     Dbcaml.Dbcaml.fetch_one
       conn
       ~params:[String "1"]
       "select * from users where ids = $1"
   with
  | Ok x ->
    let rows = Dbcaml.Row.row_to_type x in
    List.iter (fun x -> print_endline x) rows
  | Error x -> print_endline (Dbcaml.ErrorMessages.execution_error_to_string x));

  print_newline ();
  print_newline ();
  print_endline "fetching many rows...";
  print_newline ();
  (* Fetch 1 row from the database *)
  match
    Dbcaml.Dbcaml.fetch_many conn "select * from users where name LIKE 'B%'"
  with
  | Ok rows ->
    List.iter
      (fun x ->
        let rows = Dbcaml.Row.row_to_type x in
        List.iter (fun x -> print_endline x) rows;
        print_newline ())
      rows
  | Error x -> print_endline (Dbcaml.ErrorMessages.execution_error_to_string x)
