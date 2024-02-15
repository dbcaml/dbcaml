open Riot
module Row = Row
module Param = Param

type Message.t += ConnectionQuery of Dbcaml.Query.t

let start_link ?(max_connections = 10) (driver : Dbcaml.Driver.t) =
  Dbcaml.start_link ~max_connections driver

let fetch_one pid ?params query =
  let p = Param.params params in

  match Dbcaml.execute pid ~params:p query with
  | Ok rows ->
    (match rows with
    | [] -> Error Dbcaml.Result.NoRows
    | r -> Ok (List.hd r))
  | Error e -> Error e
