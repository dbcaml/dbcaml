let () =
  let open Alcotest in
  run
    "Serde Postgres deserializer test"
    [
      ("Unescape string", Escaped_values.suite);
      ("Deserialize record list", Record_list.suite);
      ("Deserialize record", Single_record.suite);
    ]
