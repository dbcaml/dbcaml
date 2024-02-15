open Riot

open Logger.Make (struct
  let namespace = ["dbcaml"; "acceptor_pool"]
end)

type ('ctx, 'config) state = {
  acceptors: int;
  handler: Driver.t;
  initial_ctx: 'ctx;
}

let start_link { acceptors; handler; initial_ctx } =
  debug (fun f -> f "Starting %d connections" acceptors);
  let child_specs =
    List.init acceptors (fun _ ->
        match Connector.child_spec handler initial_ctx with
        | Ok c -> c
        | Error _ -> failwith "unknown error")
  in
  Supervisor.start_link ~child_specs ()

let child_spec ~acceptors ~handler initial_ctx =
  let state = { acceptors; handler; initial_ctx } in
  Supervisor.child_spec start_link state
