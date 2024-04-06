open Riot
module Messages = Messages

open Logger.Make (struct
  let namespace = ["poolparty"]
end)

let start_link ~pool_size =
  let child_specs = [Manager.child_spec ~pool_size ()] in

  match Supervisor.start_link ~restart_limit:10 ~child_specs () with
  | Ok s -> s
  | Error _ -> failwith "Failed to start link"

let add_connection ~connection_manager_pid ~item =
  let _ = Holder.new_holder connection_manager_pid item in

  ()

let get_connection connection_manager_pid =
  send connection_manager_pid (Messages.LockHolder (self ()));

  match receive () with
  | Messages.HolderMessage (holder_pid, conn) -> Ok (holder_pid, conn)
  | _ -> Error "didn't get the message I expected"

let release_connection connection_manager_pid ~holder_pid =
  send connection_manager_pid (Messages.CheckIn holder_pid)
