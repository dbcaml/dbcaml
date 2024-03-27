module Bs = Bytestring

open Riot.Logger.Make (struct
  let namespace = ["dbcaml"; "dbcaml_postgres_driver"]
end)

let ( let* ) = Result.bind

module Postgres = struct
  type config = { conninfo: string }

  let connect config =
    let* (conn, conninfo) = Pg.connect config.conninfo in

    let* (message_format, startup_response) =
      Startup.start conn conninfo.user conninfo.database
    in

    let* conn =
      Authentication.authenticate
        conn
        message_format
        startup_response
        conninfo.user
        conninfo.password
    in

    let execute (_ : Pg.t) (_ : Dbcaml.Connection.param list) _ :
        (bytes, Dbcaml.Res.execution_error) Dbcaml.Res.result =
      Error (Dbcaml.Res.GeneralError "Not implemented")
    in

    (* Create a new connection which we also want to use to create a PID *)
    let* conn = Dbcaml.Connection.make ~conn ~execute () in

    Ok conn
end

let connection conninfo =
  Dbcaml.Driver.Driver { driver = (module Postgres); config = { conninfo } }
