open Riot

open Logger.Make (struct
  let namespace = ["poolparty"; "child"]
end)

let rec wait_for_job connection_manager_pid (item : 'item) =
  (match receive () with
  | Message_passing.CheckOut request_pid -> send request_pid item
  | _ -> ());

  wait_for_job connection_manager_pid item

let new_holder connection_manager_pid (item : 'item) =
  let child_pid =
    spawn_link (fun () -> wait_for_job connection_manager_pid item)
  in
  send connection_manager_pid (Message_passing.CheckIn child_pid);

  info (fun f -> f "Got a message with a type I don't know about")
