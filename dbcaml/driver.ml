open Riot

let ( let* ) = Result.bind

module type Intf = sig
  type config

  val connect : config -> (Connection.t, [> `msg of string ]) IO.io_result
end

type t =
  | Driver : {
      driver: (module Intf with type config = 'config);
      config: 'config;
    }
      -> t

module Postgres = struct
  type config = { conninfo: string }

  let connect config =
    let u = Uri.of_string config.conninfo in
    let host = Uri.host u |> Option.value ~default:"localhost" in
    let port = Uri.port u |> Option.value ~default:5432 in
    let user = Uri.userinfo u |> Option.value ~default:"postgres" in
    let password = Uri.password u |> Option.value ~default:"" in

    let c = PGOCaml.connect ~host ~port ~user ~password () in
    let execute c query =
      let name = "dbcaml." ^ Digest.to_hex (Digest.string query) in
      PGOCaml.prepare c ~name ~query ();
      let row = PGOCaml.execute c ~name ~params:[] () in

      Ok row
    in

    let* conn = Connection.make ~conn:c ~execute () in

    Ok conn
end

let postgres config = Driver { driver = (module Postgres); config }
