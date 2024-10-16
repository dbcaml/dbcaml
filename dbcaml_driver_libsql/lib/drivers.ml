(* Setup a new connection using the connection string.
     We setup different type of connections depending on the connection string.
     - If the connection string is starting with sqlite:// or sqlite3:// do we use a sqlite driver.
       - If the conninfo is sqlite::memory: do we use in-memory driver
       - If the conninfo is a relative or absolute path do we use "normal" sqlite driver
     - If the connection string is starting with libsql:// do we setup a libsql driver
*)

let ( let* ) = Result.bind

type drivers =
  | LibSQL of string
  | Sqlite of string
  | SqliteMemory

let extract_scheme_and_value url =
  let open Base.String in
  match split_on_chars ~on:[':'; '/'] url with
  | "libsql" :: _ :: _ :: rest -> Ok (LibSQL (concat ~sep:":" rest))
  (* We need to figure out how sqlite should be part of this *)
  (*| "sqlite" :: _ :: "memory" :: _ -> Ok SqliteMemory*)
  (*| "sqlite" :: _ :: _ :: rest -> Ok (Sqlite (concat ~sep:"/" rest))*)
  | "sqlite" :: _
  | "sqlite3" :: _ ->
    Error "Sqlite driver is not implemented yet"
  | _ ->
    Error
      "Couldn't find a match for the provided connection string. Should either be a local file, starting with lisql:// or be :memory:"

let make connection_string =
  let* driver = extract_scheme_and_value connection_string in

  match driver with
  | LibSQL url -> Ok (Libsql_driver.connection url)
  | Sqlite _
  | SqliteMemory ->
    Error "Sqlite driver is not implemented yet"
