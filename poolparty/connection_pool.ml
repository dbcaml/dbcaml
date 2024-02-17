open Riot

open Logger.Make (struct
  let namespace = ["poolparty"]
end)

let start_link ~pool_size =
  let connection_manager_pid =
    spawn (fun () -> Pool_manager.start_link pool_size)
  in

  connection_manager_pid

let add_item connection_manager_pid item =
  send connection_manager_pid (Message_passing.NewHolder item)

let lock connection_manager_pid child_pid =
  send connection_manager_pid (Message_passing.LockHolder child_pid)

let unlock connection_manager_pid child_pid =
  send connection_manager_pid (Message_passing.CheckIn child_pid)
