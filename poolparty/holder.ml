open Riot

open Logger.Make (struct
  let namespace = ["poolparty"; "child"]
end)

let rec wait_for_job connection_manager_pid (item : 'item) =
  (match receive () with
  (*
   * The holder waits for a CheckOut message. When the holder get a CheckOut message 
   * do it send whatever it's holding to the requester
   *)
  | Message_passing.CheckOut request_pid -> send request_pid item
  | _ -> ());

  (*
  * loop the process over and over again
  *)
  wait_for_job connection_manager_pid item

(*
* This function registers a new holder. 
* When it get's the job do it:
* 1. Create a process which loops over messages the process will receive
* 2. Send a CheckIn message to the pool_manager that it's ready to handle jobs
*)
let new_holder connection_manager_pid (item : 'item) =
  let child_pid =
    spawn_link (fun () -> wait_for_job connection_manager_pid item)
  in
  send connection_manager_pid (Message_passing.CheckIn child_pid);

  info (fun f -> f "Got a message with a type I don't know about")
