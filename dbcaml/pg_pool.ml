open Riot
open Postgresql

type query = string

type Message.t += Query of query

type Message.t += ReadyStatus of string

module PgPool = struct
  type t = { max_connections: int }

  let default = { max_connections = 10 }

  let create_connection conninfo =
    try new connection ~conninfo () with
    (* FIXME:  change this to return a proper error *)
    | _ -> failwith "failed to create connection"

  (*
    This function should create a connection pool and return the pool so we can interact with it.
    *)
  let connect ?(max_connections = 10) conninfo =
    let connection_manager_pid =
      spawn (fun () ->
          let connections =
            Array.make max_connections (create_connection conninfo)
            |> Array.to_list
          in

          Logger.debug (fun f ->
              f "Created %d connections" (List.length connections));

          (*
            * Recive a job and execute it. later on return the value back to the connection manager which will send it back to the client
            *)
          match receive () with
          | Query query ->
            let c = List.hd connections in
            c#exec query |> ignore
          | ReadyStatus status ->
            Logger.debug (fun f -> f "Got status: %s" status)
          | _ -> failwith "unknown message")
    in

    Logger.debug (fun f ->
        f "Booting Connection manager with PID: %a" Pid.pp (self ()));

    send connection_manager_pid (Query "SELECT * FROM users")
end
