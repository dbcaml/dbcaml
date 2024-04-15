open Riot

open Logger.Make (struct
  let namespace = ["examples"; "basic_postgres"]
end)

type user = {
  id: int;
  name: string;
}
[@@deriving deserialize]

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in
  set_log_level (Some Logger.Debug);
  info (fun f -> f "Starting application");

  (* Start the database connection pool *)
  let db =
    match
      let config =
        Silo.config
          ~connections:4
          ~driver:(module Dbcaml_driver_postgres)
          ~connection_string:
            "postgresql://postgres:postgres@localhost:6432/postgres?sslmode=disabled"
      in

      Silo.connect ~config
    with
    | Ok x -> x
    | Error x -> failwith x
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
  print_int fetched_user.id;

  ()
