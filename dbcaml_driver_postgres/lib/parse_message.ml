open Message_format

let ( let* ) = Result.bind

let rec parse_response message_type message =
  match message_type with
  | ReadyForQuery -> Ok message
  | ParseComplete
  | BindComplete
  | ParameterDescription
  | NoData
  | EmptyQueryResponse
  | PortalSuspended
  | CloseComplete ->
    (match Message_format.message (Bytes.of_string message) with
    | Error _ as e -> e
    | Ok (mt, m) -> parse_response mt m)
  (* FIXME: implement me, this should be some parsing *)
  | DataRow -> Ok message
  (* FIXME: implement me. used when exec queries is the one used. *)
  | CommandComplete -> Ok message
  (* FIXME: implement me. used when a pair of new rows is on the way to be sent to the query *)
  | RowDescription -> Ok message
  | m ->
    Error
      (`Msg
        (Printf.sprintf
           "Unexpected message type(%s), got message: %s"
           (Message_format.to_string ~format:m)
           message))

let wait_for_response conn =
  let* (_, message_type, message) = Pg.receive conn in

  parse_response message_type message
