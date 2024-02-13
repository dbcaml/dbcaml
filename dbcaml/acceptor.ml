open Riot

open Logger.Make (struct
  let namespace = ["dbcaml"; "acceptor"]
end)

type ('ctx, 'config) state = {
  initial_ctx: 'ctx;
  connection: Connection.t;
}

let rec handle_job conn_sup state =
  let accepted_at = Ptime_clock.now () in

  let child_spec =
    Connector.child_spec
      ~accepted_at
      ~connection:state.connection
      ~ctx:state.initial_ctx
  in

  match Dynamic_supervisor.start_child conn_sup child_spec with
  | Ok _pid -> print_endline "hello"
  | Error `Max_children ->
    debug (fun f -> f "too many conns, waiting...");
    sleep 0.100;
    handle_job conn_sup state

let start_link state =
  let pid =
    spawn_link (fun () ->
        process_flag (Trap_exit true);
        let conn_sup = Process.await_name "dbcaml.connection.sup" in
        handle_job conn_sup state)
  in
  Ok pid

let child_spec handler initial_ctx =
  match Driver.connect handler with
  | Ok connection ->
    let state = { connection; initial_ctx } in
    Ok (Supervisor.child_spec start_link state)
  | Error e -> Error e
