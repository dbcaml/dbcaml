open Riot

open Logger.Make (struct
  let namespace = ["heimdal"]
end)

let start_link ~pool_size =
  let connection_manager_pid =
    spawn (fun () -> Connection_manager.start_link pool_size)
  in

  connection_manager_pid

let add_item connection_manager_pid child_pid =
  send connection_manager_pid (Message_passing.CheckIn child_pid)

let lock connection_manager_pid child_pid =
  send connection_manager_pid (Message_passing.CheckOut child_pid)

let unlock connection_manager_pid child_pid =
  send connection_manager_pid (Message_passing.CheckIn child_pid)
