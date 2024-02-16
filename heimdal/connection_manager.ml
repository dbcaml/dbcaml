open Riot
open Hashtbl

open Logger.Make (struct
  let namespace = ["heimdal"; "connection_manager"]
end)

type ('ctx, 'item) state = {
  acceptors: int;
  item: 'item;
  initial_ctx: 'ctx;
}

let global_storage : (string, 'a) Hashtbl.t = Hashtbl.create 100

let storage_mutex = Mutex.create ()

let add_to_storage key value =
  Mutex.lock storage_mutex;
  Hashtbl.add global_storage key value;
  Mutex.unlock storage_mutex

let get_from_storage key =
  Mutex.lock storage_mutex;
  let value = Hashtbl.find_opt global_storage key in
  Mutex.unlock storage_mutex;
  value

let remove_from_storage key =
  Mutex.lock storage_mutex;
  Hashtbl.remove global_storage key;
  Mutex.unlock storage_mutex

let rec in_memory_table () =
  (match receive () with
  | Message_passing.CheckIn msg ->
    print_endline msg;
    info (fun f -> f "Got a message with a type I don't know about")
  | Message_passing.CheckOut msg ->
    print_endline msg;
    info (fun f -> f "Got a message with a type I don't know about")
  | _ -> error (fun f -> f "Got a message with a type I don't know about"));

  in_memory_table ()

(*
* 2. Create a in-memory storage where we can store PIDs and the state.
* 3. Be able to acquire a job and wait out until we have a ready item in the pool
*)
let start_link { acceptors; item; initial_ctx } =
  debug (fun f -> f "Initiating pool with %d items" acceptors);

  let connection_manager_pid = self () in

  let _ = spawn_link (fun () -> in_memory_table ()) in

  let child_specs =
    List.init acceptors (fun _ ->
        match Holder.child_spec ~item ~connection_manager_pid initial_ctx with
        | Ok c -> c
        | Error _ -> failwith "unknown error")
  in

  Supervisor.start_link ~child_specs ()

let child_spec ~acceptors ~item initial_ctx =
  let state = { acceptors; item; initial_ctx } in

  Supervisor.child_spec start_link state
