open Riot

open Logger.Make (struct
  let namespace = ["poolparty"; "connection_manager"]
end)

type ('ctx, 'item) state = {
  pool_size: int;
  initial_ctx: 'ctx;
}

let rec handle_messages connection_manager_pid global_storage storage_mutex =
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
   * LockHolder is used to allow clients to request a lock on a process
    * When this message is sent to the manager do the manager set the state of the holder to buzy
    * and then tell the holder to send whatever it holds to the client that requested a lock.
   *)
  | Message_passing.LockHolder requester_pid ->
    debug (fun f ->
        f "Acquire a holder lock for requester: %a" Pid.pp requester_pid);
    (match Storage.available_holder global_storage storage_mutex with
    | Ok c ->
      let (holder_pid, _) = c in
      Storage.add_or_replace
        global_storage
        storage_mutex
        holder_pid
        Storage.Busy;
      debug (fun f ->
          f
            "Selected and locked holder %a for requester %a"
            Pid.pp
            holder_pid
            Pid.pp
            requester_pid);
      send holder_pid (Message_passing.CheckOut requester_pid)
    | Error _ ->
      debug (fun f -> f "not enough holders, retrying...");
      send connection_manager_pid (Message_passing.LockHolder requester_pid))
  | _ -> error (fun f -> f "Got a message with a type I don't know about"));

  handle_messages connection_manager_pid global_storage storage_mutex

(*
* 2. Create a in-memory storage where we can store PIDs and the state.
* 3. Be able to acquire a job and wait out until we have a ready item in the pool
*)
let start_link { pool_size; _ } =
  debug (fun f -> f "Initiating pool with %d items" pool_size);

  let global_storage : (Pid.t, Storage.status) Hashtbl.t =
    Hashtbl.create pool_size
  in

  let storage_mutex = Mutex.create () in

  handle_messages (self ()) global_storage storage_mutex

let child_spec ~pool_size initial_ctx =
  let state = { pool_size; initial_ctx } in
  Supervisor.child_spec start_link state
