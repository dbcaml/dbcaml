open Riot
module Connection = Connection
module Driver = Driver
module Row = Row
module ErrorMessages = Error
module Query = Query
module AcceptorPool = Acceptor_pool

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
end
