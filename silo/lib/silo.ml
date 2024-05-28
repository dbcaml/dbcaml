let ( let* ) = Result.bind

module Params = Dbcaml.Params

module type Intf = sig
  val connection : string -> Dbcaml.Driver.t
end

type t =
  | Ready_to_connect of {
      driver: (module Intf);
      connections: int;
      connection_string: string;
    }
  | Connected of {
      driver: Dbcaml.Driver.t;
      connections: int;
      connection_string: string;
      conn_mgr_pid: Riot.Pid.t;
    }

(** Create a new config based on the provided params.  *)
let config ~connections ~driver ~connection_string =
  Ready_to_connect { driver; connections; connection_string }

(** 
  Start a connection to the database.
  This spins up a pool and creates the amount of connections provided in the config
*)
let connect ~config =
  match config with
  | Ready_to_connect
      { driver = (module DriverModule); connections; connection_string; _ } ->
    let connection = DriverModule.connection connection_string in
    (match Dbcaml.start_link ~connections connection with
    | Ok c ->
      Ok
        (Connected
           {
             driver = connection;
             connections;
             connection_string;
             conn_mgr_pid = c;
           })
    | Error (`Msg error_message) -> Error error_message)
  | Connected _ -> Error "You can't connect with a connected config"

(* Check if we have rows back. If we don't have rows shouldn't we try to start a deserializer as there is no data *)
let have_rows message =
  let data_row_description_length =
    Bytes.get_int32_be message 1 |> Int32.to_int
  in
  match Bytes.get message (data_row_description_length + 1) with
  | 'D' -> Some ()
  | _ -> None

(** Query send a fetch request to the database and use the bytes to deserialize the output to a type using serde. Ideal to use for select queries *)
let query :
    type value state.
    ?params:Dbcaml.Params.t list ->
    t ->
    query:string ->
    deserializer:(value, state) Serde.De.t ->
    (value option, string) result =
 fun ?params config ~query ~deserializer ->
  match config with
  | Connected { conn_mgr_pid; driver; _ } ->
    let* result = Dbcaml.raw_query conn_mgr_pid ~params ~query in
    let result_bytes = Bytes.of_string result in

    (match have_rows result_bytes with
    | Some _ ->
      (match driver with
      | Driver { driver = (module DriverIntf); _ } ->
        (match
           Dbcaml.deserialize (module DriverIntf) deserializer result_bytes
         with
        | Ok t -> Ok (Some t)
        | Error e ->
          Error (Format.asprintf "Deserialize error: %a" Serde.pp_err e)))
    | None -> Ok None)
  | Ready_to_connect _ -> Error "Should be a connected config"

(** Execute sends a execute command to the database and returns the amount of rows affected. Ideal to use for insert,update and delete queries  *)
let execute ?(params = []) config ~query =
  match config with
  | Connected { conn_mgr_pid; _ } ->
    let params =
      if List.length params > 0 then
        Some params
      else
        None
    in

    let* result = Dbcaml.raw_query conn_mgr_pid ~params ~query in
    let* rows_affected = parse_command_complete result in

    Ok rows_affected
  | Ready_to_connect _ -> Error "Should be a connected config"
