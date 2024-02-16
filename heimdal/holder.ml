open Riot

open Logger.Make (struct
  let namespace = ["heimdal"; "holder"]
end)

type ('ctx, 'item) state = {
  item: 'item;
  initial_ctx: 'ctx;
  connection_manager_pid: Pid.t;
}

(*
   * 1. checkin on start
   * 2. respond to request for usage and send a copy of the item to the pid that requests it
   * 3. Automastic checkout when we get a request and then check in to the connection manager when we're done with a job
*)
let loop state =
  print_endline "loop";
  let _ = state in

  ()

let init state = Ok (spawn_link (fun () -> loop state))

let child_spec ~item ~connection_manager_pid initial_ctx =
  let state = { item; connection_manager_pid; initial_ctx } in

  Ok (Supervisor.child_spec init state)
