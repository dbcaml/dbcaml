open Riot

type Message.t += ConnectionQuery of Query.t

open Logger.Make (struct
  let namespace = ["dbcaml"; "connector"]
end)

type ('state, 'err) state = {
  accepted_at: Ptime.t;
  connection: Connection.t;
  ctx: 'state;
}

let handle_query state =
  trace (fun f -> f "receiving process message...");
  match receive ~after:500L () with
  | exception Receive_timeout ->
    trace (fun f -> f "message timeout, trying receive...")
  (*TODO: handle this *)
  | ConnectionQuery c ->
    debug (fun f -> f "got message with query: %s" c.query);
    (match Connection.execute state.connection c.params c.query with
    | Ok rows ->
      List.iter
        (fun x ->
          let rows = Row.row_to_type x in
          List.iter (fun x -> print_endline x) rows;
          print_newline ())
        rows
    | Error e -> failwith (Error.execution_error_to_string e))
  | _msg -> error (fun f -> f "got a message type we shouldn't get")

let start_link state =
  let pid = spawn_link (fun () -> handle_query state) in
  Ok pid

let child_spec ~accepted_at ~connection ~ctx =
  let state = { accepted_at; connection; ctx } in
  Supervisor.child_spec start_link state
