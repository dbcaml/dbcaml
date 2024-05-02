open Riot

open Logger.Make (struct
  let namespace = ["examples"; "basic_postgres"]
end)

type user = {
  name: string;
  id: int;
  some_int64: int64;
  some_int32: int32;
  some_float: float;
  some_bool: bool;
  pet_name: string option;
  pets: string list;
  pets_array: string array;
}
[@@deriving deserialize]

type users = user list [@@deriving deserialize]

let () =
  Riot.run_with_status ~on_error:(fun x -> failwith x) @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in
  set_log_level (Some Logger.Debug);
  info (fun f -> f "Starting application");

  (* Start the database connection pool *)
  let* db =
    let config =
      Silo_postgres.config
        ~connections:5
        ~connection_string:
          "postgresql://postgres:postgres@localhost:6432/postgres?sslmode=disabled"
    in

    match Silo_postgres.connect ~config with
    | Ok c -> Ok c
    | Error (`Msg e) -> Error e
  in

  (* Fetch the user and return the user to a variable *)
  let* fetched_users =
    Silo_postgres.query
      db
      ~query:
        "select name, id, some_bool, pet_name, some_int64, some_int32, some_float, pets, pets as pets_array from users limit 2"
      ~deserializer:deserialize_users
  in

  List.iter
    (fun x ->
      Printf.printf
        "Fetching user with id %d:\nName: %s\nSome float: %f\nSome int64: %d\nSome int32: %d\n%s\n Some bool: %b\nPets: %s\nPets array: %s\n\n"
        x.id
        x.name
        x.some_float
        (Int64.to_int x.some_int64)
        (Int32.to_int x.some_int32)
        (match x.pet_name with
        | Some pn -> Printf.sprintf "Pet name: %S" pn
        | None -> "No pet")
        x.some_bool
        (String.concat ", " x.pets)
        (String.concat ", " (Array.to_list x.pets_array)))
    (Option.get fetched_users);

  (* Fetch the user and return the user to a variable *)
  let* fetched_users =
    Silo_postgres.query
      db
      ~query:
        "select name, id, some_bool, pet_name, some_int64, some_int32, some_float, pets, pets as pets_array from users where id < $1 limit 2"
      ~params:[Silo_postgres.Params.Number 3]
      ~deserializer:deserialize_users
  in

  List.iter
    (fun x ->
      Printf.printf
        "Fetching user with id %d:\nName: %s\nSome float: %f\nSome int64: %d\nSome int32: %d\n%s\n Some bool: %b\nPets: %s\nPets array: %s\n\n"
        x.id
        x.name
        x.some_float
        (Int64.to_int x.some_int64)
        (Int32.to_int x.some_int32)
        (match x.pet_name with
        | Some pn -> Printf.sprintf "Pet name: %S" pn
        | None -> "No pet")
        x.some_bool
        (String.concat ", " x.pets)
        (String.concat ", " (Array.to_list x.pets_array)))
    (Option.get fetched_users);

  Ok 1
