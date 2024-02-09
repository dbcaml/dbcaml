module Connection = Connection
module Driver = Driver
module Row = Row
module ErrorMessages = Error
module Param = Param

module Dbcaml = struct
  let start_link (d : Driver.t) =
    match Driver.connect d with
    | Ok connection -> Ok connection
    | Error e -> Error e

  let fetch_one connection ?params query =
    let p =
      match params with
      | Some opts -> opts
      | None -> [] |> Array.of_list
    in

    match Connection.execute connection p query with
    | Ok rows ->
      (match rows with
      | [] -> Error ErrorMessages.NoRows
      | r -> Ok (List.hd r))
    | Error _ -> Error ErrorMessages.NoRows
end
