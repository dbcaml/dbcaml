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

let () =
  Riot.run_with_status ~on_error:(fun x -> failwith x) @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in
  set_log_level (Some Logger.Info);
  info (fun f -> f "Starting application");

  (* Start the database connection pool *)
  let* db =
    let config =
      Silo_postgres.config
        ~connections:4
        ~driver:(module Dbcaml_driver_postgres)
        ~connection_string:
          "postgresql://postgres:postgres@localhost:6432/postgres?sslmode=disabled"
    in

    Silo_postgres.connect ~config
  in

  (* Fetch the user and return the user to a variable *)
  let* fetched_user =
    Silo_postgres.fetch_many
      db
      ~query:
        "select name, id, some_bool, pet_name, some_int64, some_int32, some_float, pets, pets as pets_array from users limit 2"
      ~deserializer:deserialize_user
  in

  (* Print the users name *)
  print_endline fetched_user.name;
  print_endline (string_of_float fetched_user.some_float);
  print_endline (string_of_int (Int64.to_int fetched_user.some_int64));
  print_endline (string_of_int (Int32.to_int fetched_user.some_int32));
  (match fetched_user.pet_name with
  | Some pn -> Printf.printf "pet_name: %S" pn
  | None -> print_endline "no pet");
  print_int fetched_user.id;
  Printf.printf "%b" fetched_user.some_bool;
  List.iter (fun x -> print_endline x) fetched_user.pets;
  Array.iter (fun x -> print_endline x) fetched_user.pets_array;

  Ok 1
