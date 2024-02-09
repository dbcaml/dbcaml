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

  let result =
    match
      Dbcaml.Dbcaml.fetch_one
        conn
        ~params:[| "8" |]
        "select * from users where id = $1"
    with
    | Ok x -> x
    | Error x -> failwith x
  in

  List.iter
    (fun x ->
      let rows = Dbcaml.Row.row_to_type x in
      List.iter (fun x -> print_endline x) rows;
      print_newline ())
    result;

  let _ = conn in

  ()
