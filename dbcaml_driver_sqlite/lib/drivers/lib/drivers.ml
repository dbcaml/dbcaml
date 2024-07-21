(* Setup a new connection using the connection string.
     We setup different type of connections depending on the connection string.
     - If the connection string is starting with sqlite:// or sqlite3:// do we use a sqlite driver.
       - If the conninfo is sqlite::memory: do we use in-memory driver
       - If the conninfo is a relative or absolute path do we use "normal" sqlite driver
     - If the connection string is starting with libsql:// do we setup a libsql driver
*)
let make conninfo =
  let uri = Uri.of_string conninfo in
  let scheme = Uri.scheme uri in
  let path = Uri.path uri in
  print_endline "path";
  print_endline path;
  print_endline conninfo;

  match scheme with
  | Some "libsql" -> Ok "libsql"
  | Some "sqlite" when Filename.is_relative path -> Ok "local_path"
  | Some "sqlite" when conninfo == "sqlite::memory:" -> Ok "memory"
  | Some _
  | _ ->
    Error
      (`Msg
        "Couldn't find a match for the provided connection string. Should either be a local file, starting with lisql:// or be :memory:")
