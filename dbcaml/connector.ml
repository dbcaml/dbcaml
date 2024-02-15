open Riot

open Logger.Make (struct
  let namespace = ["dbcaml"; "connector"]
end)

type ('state, 'err) state = {
  handler: Driver.t;
  initial_ctx: 'state;
}

let rec handle_query state connection =
  debug (fun f -> f "PID %a is ready to receive message..." Pid.pp (self ()));
  match receive () with
  | exception Receive_timeout ->
    debug (fun f -> f "message timeout, retrying...");

    handle_query state connection;
  | Message_passing.Query c ->
    debug (fun f -> f "got message with query: %s" c.query);

    send
      c.owner
      (Message_passing.Result
         (Connection.execute state.connection c.params c.query));

    handle_query state connection;
  | _msg ->
    error (fun f -> f "got a message type we shouldn't get");
    handle_query state connection;

let start_link state =
  let pid =
    spawn_link (fun () ->
        match Driver.connect state.handler with
        | Ok connection -> handle_query state connection
        | Error e -> Error e)
  in

  Ok pid

(* TODO: don't start connection, start in spawn_link *)
let child_spec handler initial_ctx =
  let state = { handler; initial_ctx } in
  Ok (Supervisor.child_spec start_link state)
