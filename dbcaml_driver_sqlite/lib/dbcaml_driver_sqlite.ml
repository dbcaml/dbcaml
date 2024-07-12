let ( let* ) = Result.bind

module Sqlite = struct
  type config = { conninfo: string }

  let connect _ =
    let query ~connection:_ ~params:_ ~query:_ ~row_limit:_ =
      Ok (Bytes.of_string "")
    in

    (* Create a new connection which we also want to use to create a PID *)
    let* conn = Dbcaml.Connection.make ~conn:"" ~query () in

    Ok conn

  (** Deserialize the response bytes from postgres into a type *)
  let deserialize = Serde_postgres.of_bytes
end

(** Create a interface which returns back a Dbcaml.Driver.t type. This type is used to create a connection and make queries *)
let connection conninfo =
  Dbcaml.Driver.Driver { driver = (module Sqlite); config = { conninfo } }
