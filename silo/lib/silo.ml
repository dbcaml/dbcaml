let ( let* ) = Result.bind

module Params = Dbcaml.Params

module type Intf = sig
  val connection : string -> Dbcaml.Driver.t
end

module type SerdeIntf = sig
  type t

  val of_bytes :
    ('a, Serde_postgres.Deserializer.state) Serde.De.t ->
    bytes ->
    ('a option, string) result
end

type t =
  | Config : {
      deserializer: (module SerdeIntf);
      driver: (module Intf);
      connections: int;
      connection_string: string;
    }
      -> t

(** Create a new config based on the provided params.  *)
let config ~connections ~driver ~connection_string ~deserializer =
  Config { driver; deserializer; connections; connection_string }

(** 
  Start a connection to the database.
  This spins up a pool and creates the amount of connections provided in the config
*)
let connect ~config =
  match config with
  | Config { driver = (module DriverModule); connections; connection_string; _ }
    ->
    let connection = DriverModule.connection connection_string in
    Dbcaml.start_link ~connections connection

(* Check if we have rows back. If we don't have rows shouldn't we try to start a deserializer as there is no data *)
let check_amount_rows message =
  let data_row_description_length =
    Bytes.get_int32_be message 1 |> Int32.to_int
  in
  match Bytes.get message (data_row_description_length + 1) with
  | 'D' -> Some ()
  | _ -> None

(** Query send a fetch request to the database and use the bytes to deserialize the output to a type using serde. Ideal to use for select queries *)
let query ?(params = []) connection_manager_pid ~query ~deserializer =
  let params =
    if List.length params > 0 then
      Some params
    else
      None
  in

  let* result = Dbcaml.raw_query connection_manager_pid ~params ~query in

  let result_bytes = Bytes.of_string result in
  match check_amount_rows result_bytes with
  | Some _ ->
    (match Serde_postgres.of_bytes deserializer result_bytes with
    | Ok t -> Ok (Some t)
    | Error e -> Error (Format.asprintf "Deserialize error: %a" Serde.pp_err e))
  | None -> Ok None

(** Used internally. parse_command_complete reads the "Command Complete" message which starts with a C and reads how many rows that is effected and return a int
    If it's unable to do so do it return a error *)
let parse_command_complete message =
  try
    let length = String.length message in
    (* Find the position of the last space, before the number of rows *)
    let space_pos = String.rindex message ' ' in
    (* Extract the number from space position to the end minus the null character *)
    let number_str =
      String.sub message (space_pos + 1) (length - space_pos - 2)
    in
    (* Convert the extracted string to an integer *)
    let value = int_of_string number_str in
    Ok value
  with
  | _ -> Error "failed to parse command complete message"

(** Execute sends a execute command to the database and returns the amount of rows affected. Ideal to use for insert,update and delete queries  *)
let execute ?(params = []) connection_manager_pid ~query =
  let params =
    if List.length params > 0 then
      Some params
    else
      None
  in

  let* result = Dbcaml.raw_query connection_manager_pid ~params ~query in
  let* rows_affected = parse_command_complete result in

  Ok rows_affected
