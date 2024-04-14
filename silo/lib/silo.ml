let ( let* ) = Result.bind

let start ?(connections = 10) driver = Dbcaml.start_link ~connections driver

let fetch_one ?(params = None) connection_manager_pid ~query ~deserializer =
  let* result =
    Dbcaml.raw_query connection_manager_pid ~params ~query ~row_limit:1
  in

  let result_bytes = Bytes.of_string result in

  let* (buf, _headers) = Decode.decode_row_description result_bytes in

  match Serde_postgres.of_rows deserializer buf with
  | Ok t -> Ok t
  | Error e -> Error (Format.asprintf "Deserialize error: %a" Serde.pp_err e)
