open Riot

open Logger.Make (struct
  let namespace = ["poolparty"; "connection_manager"]
end)

type ('ctx, 'item) state = {
  acceptors: int;
  initial_ctx: 'ctx;
}

let rec handle_messages global_storage storage_mutex =
  let current_pid = self () in
  (match receive () with
  (*
   * CheckIn triggers when a holder is either reigstered for the first time or 
   * have been unlocked by a process so it can go on duty again
   *)
  | Message_passing.CheckIn child_pid ->
    debug (fun f -> f "Check in pid: %a" Pid.pp child_pid);
    (* Storage the current holder PID as ready to handle jobs in our in memory table *)
    Storage.add_or_replace global_storage storage_mutex child_pid Storage.Ready
  (*
   * NewHolder triggers when a holder is registered and want to be stored in the in memory table. 
   * After holder have registered do the holder send a message to pool_manager to let it know that
   * it's ready to handle job
   *)
  | Message_passing.NewHolder item ->
    debug (fun f -> f "Storing a new holder");
    Holder.new_holder current_pid item
  (*
   * LockHolder is used to allow clients to request a lock on a process
    * When this message is sent to the manager do the manager set the state of the holder to buzy
    * and then tell the holder to send whatever it holds to the client that requested a lock.
   *)
  | Message_passing.LockHolder requester_pid ->
    debug (fun f ->
        f "Acquire a holder lock for requester: %a" Pid.pp requester_pid);
    let item = Storage.available_holder global_storage storage_mutex in
    let (holder_pid, _) = item in
    Storage.add_or_replace global_storage storage_mutex holder_pid Storage.Buzy;
    send holder_pid (Message_passing.CheckOut requester_pid)
  | _ -> error (fun f -> f "Got a message with a type I don't know about"));

  handle_messages global_storage storage_mutex

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

  handle_messages global_storage storage_mutex
