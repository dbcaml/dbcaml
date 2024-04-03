open Riot
module Connection = Connection
module Driver = Driver
module Res = Res
module Param = Param

open Logger.Make (struct
  let namespace = ["dbcaml"]
end)

(*
 * start_link is the main function for Dbcaml, starts the Supervisor which 
 * controls the Pool manager
 *)
let start_link ?(connections = 10) (driver : Driver.t) =
  let pool_id = Poolparty.start_link ~pool_size:connections in

  let pids =
    List.init connections (fun _ ->
        let pid =
          spawn (fun () ->
              match Driver.connect driver with
              | Ok c -> Poolparty.add_item pool_id c
              | Error _ -> error (fun f -> f "failed to start driver"))
        in

        pid)
  in

  wait_pids pids;

  Ok pool_id

let execute pool_id query =
  let item = Poolparty.get_holder_item pool_id |> Result.get_ok in

  let result = Connection.execute item.item query in

  Poolparty.release pool_id item.holder_pid;

  result

let fetch_one pool_id ?params query =
  let p =
    match params with
    | Some p -> p
    | None -> []
  in

  match execute pool_id p query with
  | Ok rows -> Ok rows
  | Error e -> Error e
