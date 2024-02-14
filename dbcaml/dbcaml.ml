open Riot
module Connection = Connection
module Driver = Driver
module Row = Row
module ErrorMessages = Error
module Param = Param

type Message.t += ConnectionQuery of Query.t

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

  let fetch_one pid ?params query =
    let p = Param.params params in
    send pid (ConnectionQuery { query; params = p });

    let example : Row.t = ["Hello"; "world"] in
    Ok example

  (*match Connection.execute connection (Param.params params) query with
    | Ok rows ->
      (match rows with
      | [] -> Error ErrorMessages.NoRows
      | r -> Ok (List.hd r))
    | Error e -> Error e *)
end
