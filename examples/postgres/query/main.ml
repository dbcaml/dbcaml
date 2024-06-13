open Riot

open Logger.Make (struct
  let namespace = ["examples"; "basic_postgres"]
end)

type query_result = int [@@deriving deserialize]

(* type for Users table *)
type user = {
  id: int;
  name: string;
  some_bool: bool;
  some_int64: int64;
  some_int32: int32;
  some_float: float;
  pets: string list;
  pets_array: string array;
  pet_name: string option;
}
[@@deriving deserialize]

type users = user list [@@deriving deserialize]

let () =
  Riot.run_with_status ~on_error:(fun x -> failwith x) @@ fun () ->
  let _ =
    match Logger.start () with
    | Error (`Msg e) -> failwith e
    | Error `Supervisor_error -> failwith "SUPERVISOR"
    | Error (`Application_error msg) -> failwith msg
    | Ok pid -> pid
  in
  set_log_level (Some Logger.Debug);
  info (fun f -> f "Starting application");
  let* db =
    let config =
      Silo_postgres.config
        ~connections:20
        ~connection_string:
          "postgresql://postgres:postgres@localhost:6432/postgres?sslmode=disabled"
    in
    match Silo_postgres.connect ~config with
    | Ok c -> Ok c
    | Error (`Msg e) -> Error ("connection:" ^ e)
  in
  let _ = db in
  let fetched_users =
    match
      Silo_postgres.query
        db
        ~query:
          "select id, name, some_bool, some_int64, some_int32, some_float, pets, pets as pets_array, pet_name from users limit 2"
        ~deserializer:deserialize_users
    with
    | Ok result -> Option.get result
    | Error e -> failwith e
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
    fetched_users;

  info (fun f -> f "Starting application");
  Ok 1
