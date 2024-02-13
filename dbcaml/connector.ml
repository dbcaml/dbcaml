open Riot

open Logger.Make (struct
  let namespace = ["dbcaml"; "connector"]
end)

type ('state, 'err) state = {
  accepted_at: Ptime.t;
  connection: Connection.t;
  ctx: 'state;
}

let start_link _state =
  let pid = spawn_link (fun () -> print_endline "hello") in
  Ok pid

let child_spec ~accepted_at ~connection ~ctx =
  let state = { accepted_at; connection; ctx } in
  Supervisor.child_spec start_link state
