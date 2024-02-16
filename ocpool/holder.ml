open Riot

open Logger.Make (struct
  let namespace = ["ocpool"; "holder"]
end)

type ('ctx, 'item) state = {
  item: 'item;
  initial_ctx: 'ctx;
  connection_manager_pid: Pid.t;
}

let rec loop state = loop state

let init state = Ok (spawn_link (fun () -> loop state))

let child_spec ~item ~connection_manager_pid initial_ctx =
  let state = { item; connection_manager_pid; initial_ctx } in

  Ok (Supervisor.child_spec init state)
