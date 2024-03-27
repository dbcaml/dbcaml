open Alcotest

let test_empty_string () =
  let output = Buffer_tools.Encode.string "" in
  let expected = Bytes.of_string "" in
  check bytes "empty string" expected output

let test_ascii_string () =
  let word = "Hello world" in
  let output = Buffer_tools.Encode.string word in
  let expected = Bytes.of_string word in
  check bytes "ascii string" expected output

let () =
  run
    "string encoding"
    [
      ("Empty string", [test_case "Empty string" `Quick test_empty_string]);
      ("Ascii string", [test_case "Ascii string" `Quick test_ascii_string]);
    ]
