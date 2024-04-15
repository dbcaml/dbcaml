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
  | Config (module Driver) ->
    Dbcaml.start_link ~connections:config.connections connection
  | _ -> Error "Failed to match"

let fetch_one ?(params = None) connection_manager_pid ~query ~deserializer =
  let* result =
    Dbcaml.raw_query connection_manager_pid ~params ~query ~row_limit:1
  in

  let result_bytes = Bytes.of_string result in

  let* (buf, _headers) = Decode.decode_row_description result_bytes in

  match Serde_postgres.of_rows deserializer buf with
  | Ok t -> Ok (List.hd t)
  | Error e -> Error (Format.asprintf "Deserialize error: %a" Serde.pp_err e)
