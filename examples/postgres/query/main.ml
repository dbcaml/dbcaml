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
  pets_name: string;
}
[@@deriving deserialize]

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
        ~connections:1
        ~connection_string:
          "postgresql://postgres:postgres@localhost:6432/postgres?sslmode=disabled"
    in
    match Silo_postgres.connect ~config with
    | Ok c -> Ok c
    | Error (`Msg e) -> Error ("connection:" ^ e)
  in
  let _ = db in
  let one =
    match
      Silo_postgres.query
        db
        ~query:
          "select id, name, some_bool, some_int64, some_int32, some_float, pets, pets_name"
        ~deserializer:deserialize_user
    with
    | Ok result -> Option.get result
    | Error e -> failwith e
  in
  print_endline one.pets_name;
  Fmt.pr "@.This is from riot: %d@." 1;
  info (fun f -> f "Starting application");
  info (fun f -> f "Starting application");
  info (fun f -> f "Starting application");
  info (fun f -> f "Starting application");
  Ok 1
