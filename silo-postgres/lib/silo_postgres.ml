let ( let* ) = Result.bind

module type Intf = sig
  val connection : string -> Dbcaml.Driver.t
end

type t =
  | Config : {
      driver: (module Intf);
      connections: int;
      connection_string: string;
    }
      -> t

let config ~connections ~driver ~connection_string =
  Config { driver; connections; connection_string }

let connect ~config =
  match config with
  | Config { driver = (module DriverModule); connections; connection_string } ->
    let connection = DriverModule.connection connection_string in
    Dbcaml.start_link ~connections connection

let fetch_many ?(params = None) connection_manager_pid ~query ~deserializer =
  let* result = Dbcaml.raw_query connection_manager_pid ~params ~query in

  let result_bytes = Bytes.of_string result in

  Printf.printf "%S\n" result;

  match Serde_postgres.of_bytes deserializer result_bytes with
  | Ok t -> Ok t
  | Error e -> Error (Format.asprintf "Deserialize error: %a" Serde.pp_err e)
