open Riot
module Connection = Connection
module Driver = Driver
module Row = Row
module Result = Result
module Query = Query
module AcceptorPool = Acceptor_pool

open Logger.Make (struct
  let namespace = ["dbcaml"; "dbcaml"]
end)

let start_link ?(max_connections = 10) (driver : Driver.t) =
  let child_specs =
    [
      Dynamic_supervisor.child_spec
        ~name:"dbcaml.connection.sup"
        ~max_children:max_connections
        ();
      Acceptor_pool.child_spec ~acceptors:max_connections ~handler:driver ();
    ]
  in

  Supervisor.start_link ~restart_limit:10 ~child_specs ()

let execute pid ?params query =
  let p =
    match params with
    | Some opts -> opts
    | None -> []
  in

  (* send current PID to the child so it can send the result back to this process *)
  let owner = self () in

  print_endline "i'm called";

  error (fun f -> f "owner: %a" Pid.pp owner);

  send pid (Message_passing.Query { query; params = p; owner });
  print_endline "have pid";
  error (fun f -> f "got: %a" Pid.pp pid);

  match receive () with
  | Message_passing.Result q -> q
  | _ -> Error (Result.GeneralError "unknown")
