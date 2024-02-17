open Riot

open Logger.Make (struct
  let namespace = ["basic_pool"]
end)

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in

  set_log_level (Some Logger.Debug);

  info (fun f -> f "Starting application");

  let _ = Heimdal.start_link ~acceptors:32 false |> Result.get_ok in

  sleep 10.1;
  ()
