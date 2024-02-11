open Riot

open Logger.Make (struct
  let namespace = ["dbcaml"; "acceptor"]
end)

type ('ctx, 'err) state = {
  driver: Driver.t;
  initial_ctx: 'ctx;
  handler: (module Driver.Intf with type config = 'config);
}

let rec accept_loop conn_sup state =
  match Net.Tcp_listener.accept state.socket with
  | Ok (conn, peer) -> handle_conn conn_sup state conn peer
  | Error err ->
    error (fun f -> f "Error accepting connection: %a" IO.pp_err err)

and handle_conn conn_sup state conn peer =
  let accepted_at = Ptime_clock.now () in
  trace (fun f -> f "Accepted connection: %a" Net.Addr.pp peer);

  let child_spec =
    Connector.child_spec
      ~accepted_at
      ~handler
      ~transport:state.transport
      ~ctx:state.initial_ctx
      ()
  in

  match Dynamic_supervisor.start_child conn_sup child_spec with
  | Ok _pid -> accept_loop conn_sup state
  | Error `Max_children ->
    debug (fun f -> f "too many conns, waiting...");
    sleep 0.100;
    handle_conn conn_sup state conn peer

let start_link state =
  let pid =
    spawn_link (fun () ->
        process_flag (Trap_exit true);
        let conn_sup = Process.await_name "dbcaml.connection.sup" in
        accept_loop conn_sup state)
  in
  Ok pid

let child_spec ~socket ?(buffer_size = 1_024 * 50) transport handler initial_ctx
    =
  let state = { socket; buffer_size; transport; handler; initial_ctx } in
  Supervisor.child_spec start_link state
