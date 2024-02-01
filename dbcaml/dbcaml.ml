open Riot

type query = string

type Message.t += Query of query

type Message.t += ReadyStatus of string

module Logger = Logger.Make (struct
  let namespace = ["dbcaml"]
end)

module Connection = Connection
module Driver = Driver

module Dbcaml = struct
  let start_link (driver : Driver.t) =
    match driver with
    | Driver { driver = (module DriverModule); config } ->
      (match DriverModule.connect config with
      | Ok connection ->
        (match connection with
        | Connection.C { conn; execute } ->
          let rows = execute conn "select * from users" in

          List.iter (fun _ -> ()) rows;

          ())
      | Error (`msg error_message) ->
        Logger.error (fun f -> f "Connection error: %s" error_message);

        ()
      | _ -> ())
end

(*
   TODO:
     1. Create a function that setups the connection and returns a connection object with PIDs
     2. Create a connection manager
*)
