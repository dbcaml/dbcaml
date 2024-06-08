let ( let* ) = Result.bind

module Sqlite = struct
  type config = { conninfo: string }

  let connect config =
    let query ~connection:_ ~params:_ ~query:_ ~row_limit:_ = Ok () in

    (* Create a new connection which we also want to use to create a PID *)
    let* conn = Dbcaml.Connection.make ~conn:_ ~query () in

    Ok conn
end

(** Create a interface which returns back a Dbcaml.Driver.t type. This type is used to create a connection and make queries *)
let connection conninfo =
  Dbcaml.Driver.Driver { driver = (module Sqlite); config = { conninfo } }
