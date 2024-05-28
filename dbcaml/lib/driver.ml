open Riot

open Logger.Make (struct
  let namespace = ["dbcaml"; "connection"]
end)

let ( let* ) = Result.bind

(*
 * The Intf module type is used to define the interface that a driver must implement.
 * The config type is used to create a new connection
 *)
module type Intf = sig
  type config

  type state

  val connect :
    config ->
    ( Connection.t,
      [> `Closed
      | `Connection_closed
      | `Eof
      | `Exn of exn
      | `Msg of string
      | `No_info
      | `Noop
      | `Process_down
      | `Timeout
      | `Unix_error of Unix.error
      | `Would_block
      ] )
    result

  val deserialize : ('a, state) Serde.De.t -> bytes -> ('a, Serde.error) result
end

(*
 * The driver type is a GADT that contains a module that implements the Intf module type and the config that is used to create a new connection
 * Each driver will later on implement it's own version of Driver.t
 *)
type t =
  | Driver : {
      driver: (module Intf with type config = 'config and type state = 'state);
      config: 'config;
    }
      -> t

type 'ctx state = {
  connection_manager_pid: Pid.t;
  driver: t;
}

let rec wait_for_job connection_manager_pid item =
  let holder_pid = self () in
  debug (fun f -> f "%a is waiting for job" Pid.pp holder_pid);

  (match receive_any () with
  (*
   * The holder waits for a CheckOut message. When the holder get a CheckOut message 
   * do it send whatever it's holding to the requester
   *)
  | Messages.CheckOut requester_pid ->
    debug (fun f ->
        f "Sending what i'm holding to requester %a" Pid.pp requester_pid);
    send requester_pid (Messages.HolderMessage (holder_pid, item))
  | _ -> error (fun f -> f "Unknown message"));

  (*
  * loop the process over and over again
  *)
  wait_for_job connection_manager_pid item

let start_link { connection_manager_pid; driver } =
  let* conn =
    match driver with
    | Driver { driver = (module DriverModule); config } ->
      (match DriverModule.connect config with
      | Ok e -> Ok e
      | Error e -> Error e)
  in

  let child_pid =
    spawn_link (fun () -> wait_for_job connection_manager_pid conn)
  in

  send connection_manager_pid (Messages.CheckIn child_pid);

  debug (fun f -> f "Created a new holder with pid: %a" Pid.pp child_pid);

  Ok child_pid

let child_spec connection_manager_pid (driver : t) =
  let state = { connection_manager_pid; driver } in

  Supervisor.child_spec start_link state
