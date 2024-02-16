open Riot

open Logger.Make (struct
  let namespace = ["ocpool"; "connection_manager"]
end)

type ('ctx, 'item) state = {
  acceptors: int;
  item: 'item;
  initial_ctx: 'ctx;
}

(*
* 1. Create pool item that holds something
* 2. Create a in-memory storage where we can store PIDs and the state.
* 3. Be able to acquire a job and wait out until we have a ready item in the pool
*)
let start_link { acceptors; item; initial_ctx } =
  debug (fun f -> f "Initiating pool with %d items" acceptors);

  let connection_manager_pid = self () in

  let child_specs =
    List.init acceptors (fun _ ->
        match Holder.child_spec ~item ~connection_manager_pid initial_ctx with
        | Ok c -> c
        | Error _ -> failwith "unknown error")
  in

  Supervisor.start_link ~child_specs ()

let child_spec ~acceptors ~item initial_ctx =
  let state = { acceptors; item; initial_ctx } in

  Supervisor.child_spec start_link state
