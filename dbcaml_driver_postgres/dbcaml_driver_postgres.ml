module PostgresDriver = struct
  type t = {
    host: string;
    port: int;
    user: string;
    password: string;
    database: string;
    connection: PGOCaml.t;
  }

  let fetch_one query = query
end

let start_link conninfo =
  let u = Uri.of_string conninfo in
  let host = Uri.host u |> Option.value ~default:"localhost" in
  let port = Uri.port u |> Option.value ~default:5432 in
  let user = Uri.userinfo u |> Option.value ~default:"postgres" in
  let password = Uri.password u |> Option.value ~default:"" in

  let c = PGOCaml.connect ~host ~port ~user ~password () in

  PostgresDriver.t
  = { host; port; user; password; database = ""; connection = c }
