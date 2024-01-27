module PostgresDriver = struct end

let start_link conninfo =
  let u = Uri.of_string conninfo in
  let host = Uri.host u |> Option.value ~default:"localhost" in
  let port = Uri.port u |> Option.value ~default:5432 in
  let user = Uri.userinfo u |> Option.value ~default:"postgres" in
  let password = Uri.password u |> Option.value ~default:"" in

  let c = PGOCaml.connect ~host ~port ~user ~password () in

  c
