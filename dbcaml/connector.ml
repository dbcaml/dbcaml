open Riot

open Logger.Make (struct
  let namespace = ["dbcaml"; "connector"]
end)

type ('state, 'err) state = {
  connection: Connection.t;
  initial_ctx: 'state;
}

let rec handle_query state =
  debug (fun f -> f "PID %a is ready to receive message..." Pid.pp (self ()));
  match receive () with
  | exception Receive_timeout ->
    debug (fun f -> f "message timeout, retrying...");

    handle_query state
  | Message_passing.Query c ->
    debug (fun f -> f "got message with query: %s" c.query);

    send
      c.owner
      (Message_passing.Result
         (Connection.execute state.connection c.params c.query));

    handle_query state
  | _msg ->
    error (fun f -> f "got a message type we shouldn't get");
    handle_query state

let start_link state =
  let pid = spawn_link (fun () -> handle_query state) in

  Ok pid

let child_spec handler initial_ctx =
  match Driver.connect handler with
  | Ok connection ->
    let state = { connection; initial_ctx } in
    Ok (Supervisor.child_spec start_link state)
  | Error e -> Error e
