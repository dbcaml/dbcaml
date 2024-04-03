let ( let* ) = Result.bind

let params_to_oid (params : Dbcaml.Param.t list) =
  List.map Oid.oid_of_type params

let _base64_encode input =
  Cryptokit.transform_string (Cryptokit.Base64.encode_compact ()) input

let simple_query ~conn ~query =
  let buf = Buffer.create 0 in

  (* Calculates the total length of the message to be sent. PostgreSQL protocol requires
     the length to include the size of the length field itself (4 bytes), the message type field (1 byte),
     and the length of the query string, plus a null terminator (1 byte) for the string. *)
  let message_length = 4 + 1 + String.length query in

  Buffer.add_char buf 'Q';
  Buffer.add_int32_be buf (Int32.of_int message_length);
  Buffer_tools.put_str_null buf query;

  let* _ = Pg.send conn ~buffer:buf in

  Parse_message.wait_for_response conn

let prepare ~conn ~params ~query =
  let statement_id = "dbcaml_" ^ Nonce.random_string 20 in
  let params_oid = params_to_oid params in
  let buf = Buffer.create 0 in

  Buffer.add_char buf 'P';
  (* Creates a new buffer with statement_id, the query and what the expected object ids are for the params provided*)
  Buffer_tools.put_length_prefixed buf (fun b ->
      Buffer_tools.put_str_null b statement_id;
      Buffer_tools.put_str_null b query;
      Buffer.add_int16_be b (List.length params_oid);
      List.iter (fun oid -> Buffer.add_int32_be b (Int32.of_int oid)) params_oid);

  let* _ = Pg.send conn ~buffer:buf in

  Ok statement_id

let bind ~statement_id ~params ~portal_name =
  let bind_buffer = Buffer.create 0 in

  (* We're both sending and recieving data as binary *)
  let formats = [1] in

  Buffer.add_char bind_buffer 'B';
  Buffer_tools.put_length_prefixed bind_buffer (fun buf ->
      (* Add the portal_name and a null terminator *)
      Buffer.add_string buf portal_name;
      Buffer.add_int8 buf 0;

      (* Add the statement id and a null terminator *)
      Buffer.add_string buf statement_id;
      Buffer.add_int8 buf 0;

      (* Add the amount formats and each format. A format is either text(0) or binary(1) *)
      Buffer.add_int16_be buf (List.length formats);
      List.iter (fun format -> Buffer.add_int16_be buf format) formats;

      (* Add the params length and all the values *)
      Buffer.add_int16_be buf (List.length params);
      List.iter
        (fun param -> Pg_arguments.encode_value param |> Buffer.add_buffer buf)
        params;

      (* Add result formats *)
      Buffer.add_int16_be buf (List.length formats);
      List.iter (fun format -> Buffer.add_int16_be buf format) formats);

  bind_buffer

let execute ~portal_name ~row_limit =
  let execute_buffer = Buffer.create 20 in
  Buffer.add_char execute_buffer 'E';
  Buffer_tools.put_length_prefixed execute_buffer (fun buf ->
      Buffer_tools.put_str_null buf portal_name;
      Buffer.add_int32_be buf (Int32.of_int row_limit));
  execute_buffer

let sync_database () =
  let sync_buffer = Buffer.create 20 in
  Buffer.add_char sync_buffer 'S';
  Buffer.add_int32_be sync_buffer (Int32.of_int 4);
  sync_buffer

let close_portal ~portal_name =
  let sync_buffer = Buffer.create 20 in
  Buffer.add_char sync_buffer 'C';
  Buffer_tools.put_length_prefixed sync_buffer (fun buf ->
      Buffer.add_char buf 'P';
      Buffer.add_string buf portal_name;
      Buffer.add_int8 buf 0);
  sync_buffer

let query_with_params ~conn ~statement_id ~params =
  let row_limit = 0 in
  let portal_name = "dbcaml_p_" ^ Nonce.random_string 20 in

  let bind_buffer = bind ~statement_id ~params ~portal_name in
  let execute_buffer = execute ~portal_name ~row_limit in
  let close_portal_buffer = close_portal ~portal_name in
  let sync_buffer = sync_database () in

  let buffer = Buffer.create 1024 in
  let* _ = Pg.send conn ~buffer:bind_buffer in
  let* _ = Pg.send conn ~buffer:execute_buffer in
  let* _ = Pg.send conn ~buffer:close_portal_buffer in
  let* _ = Pg.send conn ~buffer:sync_buffer in

  Buffer.add_buffer buffer bind_buffer;
  Buffer.add_buffer buffer execute_buffer;
  Buffer.add_buffer buffer close_portal_buffer;
  Buffer.add_buffer buffer sync_buffer;

  let* _ = Pg.send conn ~buffer in
  Parse_message.wait_for_response conn

let execute ~conn ~params ~query =
  match params with
  | [] ->
    let* resp = simple_query ~conn ~query in
    Ok resp
  | _ ->
    let* statement_id = prepare ~conn ~params ~query in
    query_with_params ~conn ~params ~statement_id
