open Riot
module Connection = Connection
module Driver = Driver
module Res = Res
module Params = Params

let ( let* ) = Result.bind

open Logger.Make (struct
  let namespace = ["dbcaml"]
end)

let rec wait_until_connection_exist storage =
  match Hashtbl.length storage with
  | 0 -> wait_until_connection_exist storage
  | _ -> ()

(**
 * start_link is the main function for Dbcaml, starts the Supervisor which 
 * controls the Pool manager.
 *)
let start_link ?(connections = 10) (driver : Driver.t) =
  let global_storage : (Pid.t, Storage.status) Hashtbl.t =
    Hashtbl.create connections
  in

  let pool_id =
    Pool.start_link ~pool_size:connections ~storage:global_storage
  in

  let child_specs =
    List.init connections (fun _ -> Driver.child_spec pool_id driver)
  in

  (* FIXME: we need to wait for all child to have booted before we continue *)
  let* _supervisor_pid =
    match Supervisor.start_link ~restart_limit:10 ~child_specs () with
    | Ok pid -> Ok pid
    | Error _ -> Error (`Msg "Failed to start supervisor")
  in

  wait_until_connection_exist global_storage;

  Ok pool_id

(** 
* raw_exeute send a query to the database and return raw bytes.
* It handles asking for a lock a item in the pool and releasing after query is done.
* It handles asking for a lock a item in the pool and releasing after query is done
*)
let raw_query ?(row_limit = 0) connection_manager_id ~params ~query =
  let p =
    match params with
    | Some opts -> opts
    | None -> []
  in

  let (holder_pid, connection) =
    match Pool.get_connection connection_manager_id with
    | Ok h -> h
    | Error e -> failwith e
  in

  let result =
    match Connection.query ~conn:connection ~params:p ~query ~row_limit with
    | Ok s -> Ok (Bytes.to_string s)
    | Error e -> Error (Res.execution_error_to_string e)
  in

  Pool.release_connection connection_manager_id ~holder_pid;

  result
