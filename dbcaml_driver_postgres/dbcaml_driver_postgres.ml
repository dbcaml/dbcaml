open Riot

type query = string

type Message.t += Query of query

type Message.t += ReadyStatus of string

module Logger = Logger.Make (struct
  let namespace = ["dbcaml"]
end)

let establish_single_connection conninfo =
  spawn (fun () ->
      let c =
        try new Postgresql.connection ~conninfo () with
        | e -> failwith (Printexc.to_string e)
      in

      match receive () with
      | Query query ->
        c#send_query query;

        (* TODO: fetch the response and store in a variable *)
        Logger.debug (fun f -> f "Got result: %s" res);
        ()
      | ReadyStatus status -> Logger.debug (fun f -> f "Got status: %s" status)
      | _ -> failwith "unknown message")

module Postgres = struct
  type t = { max_connections: int }

  let default = { max_connections = 10 }

  let connect ?(max_connections = 10) conninfo =
    let connection_manager_pid =
      spawn (fun () ->
          let connections =
            Array.make
              max_connections
              (try establish_single_connection conninfo with
              | e -> failwith (Printexc.to_string e))
            |> Array.to_list
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
