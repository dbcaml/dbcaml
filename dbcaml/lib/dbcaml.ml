open Riot
module Connection = Connection
module Driver = Driver
module Res = Res
module Param = Param

let ( let* ) = Result.bind

open Logger.Make (struct
  let namespace = ["dbcaml"]
end)

(*
 * start_link is the main function for Dbcaml, starts the Supervisor which 
 * controls the Pool manager
 *)
let start_link ?(connections = 10) (driver : Driver.t) =
  let pool_id = Pool.start_link ~pool_size:connections in

  let child_specs =
    List.init connections (fun _ -> Driver.child_spec pool_id driver)
  in

  let _ =
    match Supervisor.start_link ~restart_limit:10 ~child_specs () with
    | Ok _ -> ()
    | Error _ -> failwith "Failed to start supervisor"
  in

  Ok pool_id

(** raw_exeute send a query to the database and return raw bytes *)
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

  let result = Connection.query ~conn:connection ~params:p ~query ~row_limit in

  Pool.release_connection connection_manager_id ~holder_pid;

  result