open Riot
module Row = Row
module Param = Param

type Message.t += ConnectionQuery of Dbcaml.Query.t

let start_link ?(max_connections = 10) (d : Dbcaml.Driver.t) =
  Dbcaml.Dbcaml.start_link ~max_connections d

let fetch_one pid ?params query =
  let _ = Param.params params in
  let _ = pid in
  let _ = query in

  let example : Dbcaml.Row.t = ["Hello"; "world"] in
  Ok example

(*match Connection.execute connection (Param.params params) query with
  | Ok rows ->
    (match rows with
    | [] -> Error ErrorMessages.NoRows
    | r -> Ok (List.hd r))
  | Error e -> Error e *)
