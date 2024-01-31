open Riot

module type Intf = sig
  type config

  val connect : conninfo:string -> bool
end

type t =
  | Driver : {
      driver: (module Intf with type config = 'config);
      config: 'config;
    }
      -> t

module PostgresDriver = struct
  type config = { conninfo: string }

  let connect conninfo =
    let u = Uri.of_string conninfo in
    let host = Uri.host u |> Option.value ~default:"localhost" in
    let port = Uri.port u |> Option.value ~default:5432 in
    let user = Uri.userinfo u |> Option.value ~default:"postgres" in
    let password = Uri.password u |> Option.value ~default:"" in

    let c = PGOCaml.connect ~host ~port ~user ~password () in

    let conn = Connection.make ~conn:c () in

    Ok conn
end
