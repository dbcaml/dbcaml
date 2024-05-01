open Message_format

let ( let* ) = Result.bind

let read_message buf =
  let size = Bytes.get_int32_be buf 1 in
  let total_message_length = 1 + Int32.to_int size in
  let message = Bytes.sub buf 0 total_message_length in

  let new_buffer =
    Bytes.sub buf total_message_length (Bytes.length buf - total_message_length)
  in

  Ok (message, new_buffer)

(* parse_response reads the message and filter which messages to keep. Some of the messages such as CloseComplete are we not
   not interested in but we want the data in RowDescription for instance *)
let rec parse_response acc message =
  let message_type =
    match from_u8 (Bytes.get message 0) with
    | Ok v -> v
    | Error e -> raise (Error e)
  in
  match message_type with
  | DataRow
  | CommandComplete
  | RowDescription ->
    let* (message, new_buffer) = read_message message in
    if Bytes.length new_buffer > 0 then
      parse_response (acc @ [Bytes.to_string message]) new_buffer
    else
      Ok acc
  | ReadyForQuery
  | ParseComplete
  | BindComplete
  | NoData
  | EmptyQueryResponse
  | PortalSuspended
  | CloseComplete
  | ParameterDescription ->
    let* (_message, new_buffer) = read_message message in
    if Bytes.length new_buffer > 0 then
      parse_response acc new_buffer
    else
      Ok acc
  | _ -> Ok acc

let wait_for_response conn =
  let* (_, _message_type, _size, message) = Pg.receive conn in
  let* messages = parse_response [] (Bytes.of_string message) in

  Ok (String.concat String.empty messages)
