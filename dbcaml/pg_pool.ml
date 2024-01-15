open Riot
open Postgresql

type query = string

type Message.t += Query of query

type Message.t += ReadyStatus of string

module PgPool = struct
  type t = { max_connections: int }

  let default = { max_connections = 10 }

  let create_connection conninfo =
    let pid =
      spawn (fun () ->
          let _ =
            try new connection ~conninfo () with
            | _ -> failwith "failed to create connection"
          in

          (*
            * Recive a job and execute it. later on return the value back to the connection manager which will send it back to the client
            *)
          match receive () with
          | Query query ->
            Logger.info (fun f ->
                f "Pid: %a: Got query: %s" Pid.pp (self ()) query)
          | _ -> failwith "unknown message")
    in
    pid

  (*
    This function should create a connection pool and return the pool so we can interact with it.
    *)
  let connect ?(max_connections = 10) conninfo =
    let connection_manager_pid =
      spawn (fun () ->
          let pids =
            Array.make max_connections (create_connection conninfo)
            |> Array.to_list
          in
          Logger.debug (fun f -> f "Created %d connections" (List.length pids));

          (*
            * Recive a job and execute it. later on return the value back to the connection manager which will send it back to the client
            *)
          match receive () with
          | Query query ->
            List.iter
              (fun pid ->
                Logger.debug (fun f -> f "Sending query to pid: %a" Pid.pp pid);
                send pid (Query query))
              pids
          | ReadyStatus status ->
            Logger.debug (fun f -> f "Got status: %s" status)
          | _ -> failwith "unknown message")
    in

    Logger.debug (fun f ->
        f "Booting Connection manager with PID: %a" Pid.pp (self ()));

    send connection_manager_pid (Query "SELECT * FROM users")
end
