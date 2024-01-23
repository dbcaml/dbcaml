open Postgresql

module Connection = struct
  let connect ~host ~port ~user ~password ~dbname =
    match new Postgresql.connection ~host ~port ~user ~password ~dbname () with
    | exception Postgresql.Error err -> Error err
    | db -> Ok db
end
