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

  let pids =
    List.init connections (fun _ ->
        let pid =
          spawn (fun () ->
              match Driver.connect driver with
              | Ok c ->
                Pool.add_connection ~connection_manager_pid:pool_id ~item:c
              | Error _ -> error (fun f -> f "failed to start driver"))
        in

        pid)
  in

  wait_pids pids;

  Ok pool_id

(** raw_execute send a query to the database and return raw bytes *)
let raw_execute connection_manager_id params query =
  let* (holder_pid, connection) = Pool.get_connection connection_manager_id in

  let result = Connection.execute connection params query in

  Pool.release_connection connection_manager_id ~holder_pid;

  Ok result
