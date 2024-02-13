open Riot

module Logger = Logger.Make (struct
  let namespace = ["dbcaml"; "acceptor_pool"]
end)

type 'config state = {
  acceptors: int;
  handler: (module Driver.Intf with type config = 'config);
}

let start_link { acceptors; handler } initial_ctx =
  Logger.debug (fun f -> f "Starting %d connections" acceptors);
  let child_specs =
    List.init acceptors (fun _ -> Acceptor.child_spec handler initial_ctx)
  in
  Supervisor.start_link ~child_specs ()

(*
let child_spec ~acceptors ~handler initial_ctx =
  let state = { acceptors; handler } in
  Supervisor.child_spec start_link state
  *)
