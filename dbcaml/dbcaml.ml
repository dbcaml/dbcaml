open Riot
module Connection = Connection
module Driver = Driver
module Row = Row
module Execution_result = Execution_result
module Query = Query

open Logger.Make (struct
  let namespace = ["dbcaml"]
end)

let start_link ?(connections = 2) (driver : Driver.t) =
  let pool_id = Poolparty.start_link ~pool_size:connections in

  let _ =
    List.init connections (fun _ ->
        let _ =
          spawn (fun () ->
              match Driver.connect driver with
              | Ok c -> Poolparty.add_item pool_id c
              | Error _ -> error (fun f -> f "failed to start driver"))
        in

        ())
  in

  Ok pool_id

let fetch_one pool_id ?params query =
  let p =
    match params with
    | Some opts -> opts
    | None -> []
  in

  let item = Poolparty.get_holder_item pool_id |> Result.get_ok in

  let result =
    match Connection.execute item.item p query with
    | Ok rows ->
      (match rows with
      | [] -> Error Execution_result.NoRows
      | r -> Ok (List.hd r))
    | Error e -> Error e
  in

  Poolparty.release pool_id item.holder_pid;

  result
