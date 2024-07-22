(* Setup a new connection using the connection string.
     We setup different type of connections depending on the connection string.
     - If the connection string is starting with sqlite:// or sqlite3:// do we use a sqlite driver.
       - If the conninfo is sqlite::memory: do we use in-memory driver
       - If the conninfo is a relative or absolute path do we use "normal" sqlite driver
     - If the connection string is starting with libsql:// do we setup a libsql driver
*)

let extract_scheme_and_value url =
  match String.split_on_char ':' url with
  | scheme :: "//" :: value :: _ -> (scheme, value)
  | scheme :: value :: _ -> (scheme, value)
  | _ -> ("", "")

let make connection_string =
  let (scheme, path) = extract_scheme_and_value connection_string in
  Printf.printf
    "path: %s %s %b \n"
    path
    connection_string
    (Filename.is_relative path);

  match scheme with
  | "libsql" -> Ok "libsql"
  | "sqlite" when Filename.is_relative path -> Ok "local_path"
  | "sqlite" when connection_string == "sqlite::memory:" -> Ok "memory"
  | _ ->
    Error
      (`Msg
        "Couldn't find a match for the provided connection string. Should either be a local file, starting with lisql:// or be :memory:")
