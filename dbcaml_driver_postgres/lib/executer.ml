let ( let* ) = Result.bind

open Messages

let params_to_oid (params : Dbcaml.Param.t list) =
  List.map Oid.oid_of_type params

let _base64_encode input =
  Cryptokit.transform_string (Cryptokit.Base64.encode_compact ()) input

let query_with_params ~conn ~query ~row_limit ~params =
  let statement_id = "dbcaml_" ^ Nonce.random_string 20 in
  let portal_name = "dbcaml_p_" ^ Nonce.random_string 20 in
  let params_oid = params_to_oid params in
  let prepare_buffer =
    Messages.Prepare.prepare ~statement_id ~params_oid ~query
  in

  let* _ = Pg.send conn ~buffer:prepare_buffer in

  let bind_buffer = Bind.bind ~statement_id ~params ~portal_name in
  let execute_buffer = Execute.execute ~portal_name ~row_limit in
  let close_portal_buffer = Portal.close ~portal_name in
  let sync_buffer = Sync.database () in

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

let query ~conn ~params ~query ~row_limit =
  match params with
  | [] ->
    let* _ = Pg.send conn ~buffer:(Query.query ~query) in

    Parse_message.wait_for_response conn
  | _ -> query_with_params ~conn ~params ~row_limit ~query
