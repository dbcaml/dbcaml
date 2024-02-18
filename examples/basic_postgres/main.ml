open Riot

open Logger.Make (struct
  let namespace = ["Dbcaml example"]
end)

let rec run_single_query index conn =
  info (fun f -> f "Running query for PID: %a" Pid.pp (self ()));
  let _ =
    match
      Dbcaml.fetch_one conn ~params:["1"] "select * from users where id = $1"
    with
    | Ok x ->
      info (fun f -> f "Executed query %d for pid %a" index Pid.pp (self ()));
      let rows = Dbcaml.Row.row_to_type x in
      (* Iterate over each column and print it's values *)
      List.iter (fun x -> print_endline x) rows
    | Error x -> print_endline (Dbcaml.Res.execution_error_to_string x)
  in

  if index <= 100 then (
    let _ = run_single_query (index + 1) conn in

    Unix.sleep 1;

    ()
  )

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in

  Logger.set_log_level (Some Logger.Info);

  Logger.info (fun f -> f "Starting application");

  let driver =
    Dbcaml_driver_postgres.connection
      "postgresql://postgres:mysecretpassword@localhost:6432/development"
  in

  let pool_id = Dbcaml.start_link ~connections:10 driver |> Result.get_ok in

  let _ =
    List.init 20 (fun _ ->
        let _ =
          spawn (fun () ->
              let _ = run_single_query 0 pool_id in
              ())
        in
        ())
  in

  sleep 15.0;
  ()
