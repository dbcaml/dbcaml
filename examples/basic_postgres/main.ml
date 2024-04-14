open Riot

open Logger.Make (struct
  let namespace = ["examples"; "basic_postgres"]
end)

type user = {
  id: int;
  name: string;
}
[@@deriving serialize, deserialize]

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in
  set_log_level (Some Logger.Debug);
  info (fun f -> f "Starting application");

  (* Start the database connection pool *)
  let pool_id =
    match
      Silo.start
        ~connections:4
        (Dbcaml_driver_postgres.connection
           "postgresql://postgres:postgres@localhost:6432/postgres?sslmode=disabled")
    with
    | Ok conn -> conn
    | Error e -> failwith e
  in

  (* Fetch the user and return the user to a variable *)
  let fetched_user =
    match
      Silo.fetch_one
        pool_id
        ~query:"select name, id from users limit 10"
        ~deserializer:deserialize_user
    with
    | Ok x -> x
    | Error x -> failwith x
  in

  (* Print the users name *)
  print_endline fetched_user.name;

  ()
