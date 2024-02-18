open Riot

open Logger.Make (struct
  let namespace = ["poolparty"; "holder"]
end)

let rec wait_for_job connection_manager_pid (item : 'item) =
  let holder_pid = self () in
  debug (fun f -> f "%a is waiting for job" Pid.pp holder_pid);

  (match receive () with
  (*
   * The holder waits for a CheckOut message. When the holder get a CheckOut message 
   * do it send whatever it's holding to the requester
   *)
  | Message_passing.CheckOut requester_pid ->
    debug (fun f ->
        f "Sending what i'm holding to requester %a" Pid.pp requester_pid);
    send requester_pid (Message_passing.HolderMessage { item; holder_pid })
  | _ -> error (fun f -> f "Unknown message"));

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

  debug (fun f -> f "Created a new holder with pid: %a" Pid.pp child_pid)
