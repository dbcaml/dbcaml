open Riot
(*
 * The Intf module type is used to define the interface that a driver must implement.
 * The config type is used to create a new connection
 *)

module type Intf = sig
  type config

  val connect : config -> (Connection.t, [> `msg of string ]) IO.io_result
end

(*
 * The driver type is a GADT that contains a module that implements the Intf module type and the config that is used to create a new connection
 * Eeach driver will later on implement it's own version of Driver.t
 *)
type t =
  | Driver : {
      driver: (module Intf with type config = 'config);
      config: 'config;
    }
      -> t

let connect (d : t) =
  match d with
  | Driver { driver = (module DriverModule); config } ->
    (match DriverModule.connect config with
    | Ok e -> Ok e
    | Error e -> Error e)
