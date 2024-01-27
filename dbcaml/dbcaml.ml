open Riot

type query = string

type Message.t += Query of query

type Message.t += ReadyStatus of string

module Logger = Logger.Make (struct
  let namespace = ["dbcaml"]
end)

let connection_worker c =
  spawn (fun () ->
      match receive () with
      | Query query ->
        Logger.debug (fun f -> f "Sending query: %s" query);
        ()
      | _ -> failwith "unknown message")

module DBCaml = struct
  let start_link ?(max_connections = 10) driver =
    let connection_manager_pid =
      spawn (fun () ->
          let connections =
            Array.make max_connections print_int |> Array.to_list
          in

          Logger.debug (fun f ->
              f "Created %d connections" (List.length connections));
          match receive () with
          | Query query ->
            let c = List.hd connections in
            Logger.debug (fun f -> f "Sending query to connection: %a" Pid.pp c);
            send c (Query query)
          | ReadyStatus status ->
            Logger.debug (fun f -> f "Got status: %s" status)
          | _ -> failwith "unknown message")
    in

    Logger.debug (fun f ->
        f "Booting Connection manager with PID: %a" Pid.pp (self ()));

    send connection_manager_pid (Query "SELECT * FROM users")
end
