open Riot

open Logger.Make (struct
  let namespace = ["dbcaml"; "connector"]
end)

type ('state, 'err) state = {
  accepted_at: Ptime.t;
  connection: Connection.t;
  ctx: 'state;
}

let rec handle_query state =
  debug (fun f -> f "received message from acceptor");
  match receive () with
  | exception Receive_timeout ->
    error (fun f -> f "message timeout, retrying...");
    handle_query state
  | Message_passing.Query c ->
    debug (fun f -> f "got message with query: %s" c.query);
    send
      c.owner
      (Message_passing.Result
         (Connection.execute state.connection c.params c.query))
  | _msg -> error (fun f -> f "got a message type we shouldn't get")

let start_link state =
  let pid = spawn_link (fun () -> handle_query state) in
  Ok pid

let child_spec ~accepted_at ~connection ~ctx =
  let state = { accepted_at; connection; ctx } in
  Supervisor.child_spec start_link state
