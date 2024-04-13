let ( let* ) = Result.bind

let start ?(connections = 10) conninfo =
  let driver = Dbcaml_driver_postgres.connection conninfo in

  Dbcaml.start_link ~connections driver

let fetch_one ?(params = None) connection_manager_pid ~query =
  let result =
    Dbcaml.raw_query connection_manager_pid ~params ~query ~row_limit:1
  in

  result

let to_type message =
  let* (buffer, headers) =
    Decode.decode_row_description (Bytes.of_string message)
  in

  List.iter (fun (x : Decode.field) -> print_string x.name) headers;

  Printf.printf "%S" (Bytes.to_string buffer);

  Ok (Bytes.of_string "")
