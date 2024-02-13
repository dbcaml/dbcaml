open Riot
module Connection = Connection
module Driver = Driver
module Row = Row
module ErrorMessages = Error
module Param = Param

module Dbcaml = struct
  let start_link ?(max_connections = 10) (d : Driver.t) =
    let child_specs =
      [
        Dynamic_supervisor.child_spec
          ~name:"dbcaml.connection.sup"
          ~max_children:max_connections
          ();
        Acceptor_pool.child_spec ~acceptors:max_connections ~handler:d ();
      ]
    in

    Supervisor.start_link ~restart_limit:10 ~child_specs ()

  let fetch_one connection ?params query =
    match Connection.execute connection (Param.params params) query with
    | Ok rows ->
      (match rows with
      | [] -> Error ErrorMessages.NoRows
      | r -> Ok (List.hd r))
    | Error e -> Error e

  let fetch_many connection ?params query =
    match Connection.execute connection (Param.params params) query with
    | Ok rows -> Ok rows
    | Error e -> Error e

  let exec connection ?params query =
    match Connection.execute connection (Param.params params) query with
    | Ok _ -> Ok ()
    | Error e -> Error e
end
