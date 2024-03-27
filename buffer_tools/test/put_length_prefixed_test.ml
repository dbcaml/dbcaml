open Alcotest

let test_put_length_prefixed_hello_world () =
  let buffer = Buffer.create 100 in

  let message = "Hello World" in

  let expected = "\000\000\000\015" ^ message in

  Buffer_tools.put_length_prefixed buffer (fun b -> Buffer.add_string b message);

  let result = Buffer.contents buffer in

  check string "Hello world have the correct length" expected result

let test_put_length_prefixed_database_string () =
  let buffer = Buffer.create 100 in

  let expected =
    "\000\000\000,\000\003\000\000user\000postgres\000database\000development\000\000"
  in

  Buffer_tools.put_length_prefixed buffer (fun b ->
      Buffer.add_string b "\000\003\000\000";
      Buffer_tools.put_str_null b "user";
      Buffer_tools.put_str_null b "postgres";
      Buffer_tools.put_str_null b "database";
      Buffer_tools.put_str_null b "development";
      Buffer.add_char b '\000');

  let result = Buffer.contents buffer in

  check string "Database string" expected result

let () =
  Alcotest.run
    "Prefix length"
    [
      ( "Prefix length",
        [
          ( "put_length_prefixed_hello_world",
            `Quick,
            test_put_length_prefixed_hello_world );
          ( "put_length_prefixed_database_string",
            `Quick,
            test_put_length_prefixed_database_string );
        ] );
    ]
