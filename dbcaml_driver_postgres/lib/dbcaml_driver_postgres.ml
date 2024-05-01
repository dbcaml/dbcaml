module Bs = Bytestring

let ( let* ) = Result.bind

module Postgres = struct
  type config = { conninfo: string }

  let connect config =
    let* (conn, conninfo) = Pg.connect config.conninfo in

    Pg_logger.debug "Sending startup message";
    let* _ =
      Pg.send
        conn
        ~buffer:
          (Messages.Startup.start
             ~username:conninfo.user
             ~database:conninfo.database)
    in

    let* (_, message_format, _size, _) = Pg.receive conn in

    let* _ =
      match message_format with
      | Message_format.Authentication ->
        let* _ =
          Scram_auth.authenticate
            ~conn
            ~is_plus:false
            ~username:conninfo.user
            ~password:conninfo.password
        in

        Ok conn
      | mf ->
        Error
          (`Msg
            (Printf.sprintf
               "Unexpected message format: %s"
               (Message_format.to_string ~format:mf)))
    in

    let query ~connection ~params ~query ~row_limit =
      match Executer.query ~conn:connection ~query ~row_limit ~params with
      | Ok response -> Ok response
      | Error (`Msg e) -> Error (Dbcaml.Res.ExecutionError e)
      | Error _ -> Error (Dbcaml.Res.ExecutionError "Unknown error")
    in

    (* Create a new connection which we also want to use to create a PID *)
    let* conn = Dbcaml.Connection.make ~conn ~query () in

    Ok conn
end

let connection conninfo =
  Dbcaml.Driver.Driver { driver = (module Postgres); config = { conninfo } }
