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
          let c =
            try new connection ~conninfo () with
            | _ -> failwith "failed to create connection"
          in

          (*
            * Recive a job and execute it. later on return the value back to the connection manager which will send it back to the client
            *)
          match receive () with
          | Query query ->
            let result = c#exec query in
            send (self ()) result;
            loop ()
          | _ -> failwith "unknown message" ())
    in
    pid

  (*
    This function should create a connection pool and return the pool so we can interact with it.
    *)
  let connect ?(max_connections = 10) =
    Riot.run @@ fun () ->
    for i = 0 to max_connections do
      let pid =
        spawn (fun () ->
            match receive () with
            | Query "" ->
              (*
              TODO: This is where we would create a connection pool and use it. We also need to make sure that it's possible to send queries
               *)
              shutdown ()
            | _ -> failwith "unknown message")
      in
      send pid (Query "Hello world")
    done
end
