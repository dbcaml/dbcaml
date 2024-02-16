open Riot

open Logger.Make (struct
  let namespace = ["heimdal"]
end)

let start_link ?(max_connections = 10) item =
  let child_specs =
    [
      Dynamic_supervisor.child_spec
        ~name:"heimdal.connection.sup"
        ~max_children:max_connections
        ();
      Connection_manager.child_spec ~acceptors:max_connections ~item ();
    ]
  in

  Supervisor.start_link ~restart_limit:10 ~child_specs ()
