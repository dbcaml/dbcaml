open Riot

type Message.t += Hello_world

module PgPool = struct
  type t = { max_connections: int }

  let default = { max_connections = 10 }

  let connect () =
    (*
        This function should create a connection pool and return the pool so we can interact with it.
    *)
    Riot.run @@ fun () ->
    let pid =
      spawn (fun () ->
          match receive () with
          | Hello_world ->
            (*
              TODO: This is where we would create a connection pool and use it. We also need to make sure that it's possible to send queries
               *)
            shutdown ()
          | _ -> failwith "unknown message")
    in
    send pid Hello_world
end
