open Riot
module Connection = Connection
module Driver = Driver
module Row = Row
module Result = Result
module Query = Query

open Logger.Make (struct
  let namespace = ["dbcaml"]
end)

let start_link ?(connections = 10) (driver : Driver.t) =
  let pool = Heimdal.start_link ~acceptors:connections in

  let _ =
    List.init connections (fun _ ->
        let _ =
          spawn (fun () ->
              match Driver.connect driver with
              | Ok _ -> ()
              | Error _ -> error (fun f -> f "failed to start driver"))
        in

        ())
  in

  let _ = pool in

  ()

let execute pid ?params query =
  let p =
    match params with
    | Some opts -> opts
    | None -> []
  in

  (* send current PID to the child so it can send the result back to this process *)
  let owner = self () in

  print_endline "i'm called";

  error (fun f -> f "owner: %a" Pid.pp owner);

  send pid (Message_passing.Query { query; params = p; owner });
  print_endline "have pid";
  error (fun f -> f "got: %a" Pid.pp pid);

  match receive () with
  | Message_passing.Result q -> q
  | _ -> Error (Result.GeneralError "unknown")
