open Riot
module Connection = Connection
module Driver = Driver

module Dbcaml = struct
  let start_link (d : Driver.t) =
    Logger.debug (fun f -> f "Starting application");
    let driver_connection = Driver.connect d in

    Logger.debug (fun f -> f "Connecting to the database");
    match driver_connection with
    | Ok connection ->
      let rows = Connection.execute connection "select * from users" in

      List.iter
        (fun (x : Row.t) ->
          let _ = Row.map_to x in
          print_newline ())
        rows;

      ()
    | Error _ -> print_endline "failed"
end

(*
   TODO:
     1. Create a function that setups the connection and returns a connection object with PIDs
     2. Create a connection manager
*)
