open Riot
open Postgresql

module PgPool = struct
  type t = { max_connections: int }

  type query = string

  type Message.t += Query of query

  let default = { max_connections = 10 }

  let create_connection ~conninfo =
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
          | Query query -> print_string query
          | _ -> failwith "unknown message")
    in
    pid

  (*
    This function should create a connection pool and return the pool so we can interact with it.
    *)
  let connect ?(max_connections = 10) conninfo =
    Printf.printf "Connection manager booted\n%!";

    let connection_manager_pid =
      spawn (fun () ->
          let pids = Array.make max_connections (create_connection ~conninfo) in

          print_int (Array.length pids);

          match receive () with
          | Query query -> Printf.printf "Query: %s\n%!" query
          | _ -> failwith "unknown message")
    in

    send connection_manager_pid (Query "SELECT * FROM users")
end
