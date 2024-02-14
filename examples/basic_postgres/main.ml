open Riot

let rec run_single_query index conn =
  Printf.printf "Running query %d\n" index;
  let _ =
    match
      Dbcaml.Dbcaml.fetch_one
        conn
        ~params:["1"]
        "select * from users where id = $1"
    with
    | Ok x ->
      let rows = Dbcaml.Row.row_to_type x in
      (* Iterate over each column and print it's values *)
      List.iter (fun x -> print_endline x) rows
    | Error x ->
      print_endline (Dbcaml.ErrorMessages.execution_error_to_string x)
  in

  if index <= 100 then (
    let _ = run_single_query (index + 1) conn in

    Unix.sleep 1;

    ()
  )

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

  let _ = run_single_query 0 conn in

  ()
