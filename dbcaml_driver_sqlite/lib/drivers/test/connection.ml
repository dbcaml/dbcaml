open Alcotest

let make_connection conninfo =
  match Drivers.make conninfo with
  | Ok c -> Ok c
  | Error (`Msg error_message) -> Error error_message
  | _ ->
    Alcotest.fail
      "we should't come to this place, we got a GADT we don't expect to get"

let test_make_with_libsql_conninfo () =
  check
    (result string string)
    "test_make_with_libsql_conninfo"
    (Ok "libsql")
    (make_connection "libsql://localhost:5125")

let test_make_with_local_file_conninfo () =
  check
    (result string string)
    "test_make_with_local_file_conninfo"
    (Ok "local_path")
    (make_connection "sqlite://example.db");

  check
    (result string string)
    "test_make_with_local_file_conninfo_with_relative_path"
    (Ok "local_path")
    (make_connection "sqlite://path/to/example.sqlite");

  check
    (result string string)
    "test_make_with_local_file_conninfo_with_relative_path_starting_slash"
    (Ok "local_path")
    (make_connection "sqlite://./path/to/example.sqlite");

  check
    (result string string)
    "test_make_with_local_file_conninfo_absolute_path"
    (Ok "local_path")
    (make_connection "sqlite:///var/www/example.sqlite");

  check
    (result string string)
    "test_make_with_local_file_conninfo_relative_path"
    (Ok "local_path")
    (make_connection "sqlite://./example.sqlite")

let test_make_with_memory_conninfo () =
  check
    (result string string)
    "test_make_with_memory_conninfo"
    (Ok "memory")
    (make_connection "sqlite::memory:")

let test_make_with_invalid_conninfo () =
  check
    (result string string)
    "test_make_with_invalid_conninfo"
    (Error
       "Couldn't find a match for the provided connection string. Should either be a local file, starting with lisql:// or be :memory:")
    (make_connection "invalid_conninfo")

let suite =
  [
    ("test_make_with_libsql_conninfo", `Quick, test_make_with_libsql_conninfo);
    ( "test_make_with_local_file_conninfo",
      `Quick,
      test_make_with_local_file_conninfo );
    ("test_make_with_memory_conninfo", `Quick, test_make_with_memory_conninfo);
    ("test_make_with_invalid_conninfo", `Quick, test_make_with_invalid_conninfo);
  ]
