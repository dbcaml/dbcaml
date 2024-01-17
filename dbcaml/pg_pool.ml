open Riot

type query = string

type Message.t += Query of query

type Message.t += ReadyStatus of string

module PgPool = struct
  type t = { max_connections: int }

  let default = { max_connections = 10 }

  let connect ~conninfo =
    let pid = spawn (fun () -> Connection.Default.connect conninfo) in

    pid

  (*
    This function should create a connection pool and return the pool so we can interact with it.
    *)
  let connect ?(max_connections = 10) conninfo =
    let connection_manager_pid =
      spawn (fun () ->
          let connections =
            Array.make max_connections (connect ~conninfo) |> Array.to_list
          in

          Logger.debug (fun f ->
              f "Created %d connections" (List.length connections));

          (*
          * Recive a job and execute it. later on return the value back to the connection manager which will send it back to the client
          *)
          match receive () with
          | Query query ->
            let _ = List.hd connections in
            print_string query
          | ReadyStatus status ->
            Logger.debug (fun f -> f "Got status: %s" status)
          | _ -> failwith "unknown message")
    in

    Logger.debug (fun f ->
        f "Booting Connection manager with PID: %a" Pid.pp (self ()));

    send connection_manager_pid (Query "SELECT * FROM users")
end
