open Riot

open Logger.Make (struct
  let namespace = ["heimdal"; "connection_manager"]
end)

type ('ctx, 'item) state = {
  acceptors: int;
  initial_ctx: 'ctx;
}

let rec in_memory_table global_storage storage_mutex =
  (match receive () with
  | Message_passing.CheckIn child_pid ->
    debug (fun f -> f "Check in pid: %a" Pid.pp child_pid);
    Storage.add global_storage storage_mutex child_pid Storage.Ready
  | Message_passing.NewHolder item -> Holder.new_holder (self ()) item
  | Message_passing.CheckOut child ->
    (*
        2. Take the function we have and send it back to the function who called it
       *)
    Storage.change global_storage storage_mutex child.child_pid Storage.Buzy;
    send child.child_pid (Message_passing.CheckedOut child.request_pid)
  | _ -> error (fun f -> f "Got a message with a type I don't know about"));

  in_memory_table global_storage storage_mutex

(*
* 2. Create a in-memory storage where we can store PIDs and the state.
* 3. Be able to acquire a job and wait out until we have a ready item in the pool
*)
let start_link acceptors =
  debug (fun f -> f "Initiating pool with %d items" acceptors);

  let global_storage : (Pid.t, Storage.status) Hashtbl.t =
    Hashtbl.create acceptors
  in

  let storage_mutex = Mutex.create () in

  let _ = self () in

  in_memory_table global_storage storage_mutex
