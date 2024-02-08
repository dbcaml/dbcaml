module Connection = Connection
module Driver = Driver
module Row = Row

module Dbcaml = struct
  let start_link (d : Driver.t) =
    match Driver.connect d with
    | Ok connection -> Ok connection
    | Error e ->
      Error
        (match e with
        | `msg msg -> msg
        | _ -> "unknown error")

  let fetch_one connection ?params query =
    let p =
      match params with
      | Some opts -> opts
      | None -> [] |> Array.of_list
    in

    let rows = Connection.execute connection p query in

    match rows with
    | Ok l -> Ok l
    | e -> Error e
end
