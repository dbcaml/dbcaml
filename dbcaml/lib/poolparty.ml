open Riot

open Logger.Make (struct
  let namespace = ["poolparty"]
end)

let start_link ~pool_size =
  let child_specs = [Manager.child_spec ~pool_size ()] in

  match Supervisor.start_link ~restart_limit:10 ~child_specs () with
  | Ok s -> s
  | Error _ -> failwith "unknown error"

let add_item connection_manager_pid item =
  let _ = Holder.new_holder connection_manager_pid item in

  ()

let get_holder_item connection_manager_pid =
  send connection_manager_pid (Message_passing.LockHolder (self ()));

  match receive () with
  | Message_passing.HolderMessage item -> Ok item
  | _ -> Error "Unknown error"

let release connection_manager_pid child_pid =
  send connection_manager_pid (Message_passing.CheckIn child_pid)
