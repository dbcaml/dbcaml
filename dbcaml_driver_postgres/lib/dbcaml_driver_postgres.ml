module Bs = Bytestring
module Pg_arguments = Pg_arguments

let ( let* ) = Result.bind

module Postgres = struct
  type config = { conninfo: string }

  let connect config =
    let* (conn, conninfo) = Pg.connect config.conninfo in

    let* (message_format, _) =
      Startup.start ~conn ~username:conninfo.user ~database:conninfo.database
    in

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

    let execute (_ : Pg.t) (params : Dbcaml.Param.t list) query :
        (bytes, Dbcaml.Res.execution_error) Dbcaml.Res.result =
      match Executer.execute ~conn ~query ~params with
      | Ok response -> Ok (Bytes.of_string response)
      | Error (`Msg e) -> Error (Dbcaml.Res.ExecutionError e)
      | Error _ -> Error (Dbcaml.Res.ExecutionError "Unknown error")
    in

    (* Create a new connection which we also want to use to create a PID *)
    let* conn = Dbcaml.Connection.make ~conn ~execute () in

    Ok conn
end

let connection conninfo =
  Dbcaml.Driver.Driver { driver = (module Postgres); config = { conninfo } }
