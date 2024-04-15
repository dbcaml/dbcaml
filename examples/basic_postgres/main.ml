open Riot

let ( let* ) = Result.bind

open Logger.Make (struct
  let namespace = ["examples"; "basic_postgres"]
end)

type user = {
  id: int;
  name: string;
}
[@@deriving serializer, deserializer]

let () =
  Riot.run_with_status @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in
  set_log_level (Some Logger.Debug);
  info (fun f -> f "Starting application");

  (* Start the database connection pool *)
  let* db =
    (* create Silo config *)
    let config =
      Silo.config
        ~connections:4
        ~driver:(module Dbcaml_driver_postgres)
        "postgresql://postgres:postgres@localhost:6432/postgres?sslmode=disabled"
        ()
    in

    (* connect to db *)
    Silo.connect config
  in

  (* Fetch the user and return the user to a variable *)
  let fetched_user =
    match
      Silo.fetch_one
        db
        ~query:"select name, id from users limit 10"
        ~deserializer:deserialize_user
    with
    | Ok x -> x
    | Error x -> failwith x
  in

  (* Print the users name *)
  print_endline fetched_user.name;

  ()
