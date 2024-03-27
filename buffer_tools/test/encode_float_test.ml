open Alcotest

let test_float_zero () =
  let output = Buffer_tools.Encode.float 0.0 in
  let expected = Bytes.of_string "\000\000\000\000\000\000\000\000" in
  check bytes "Float is 0.0" expected output

let test_float_one () =
  let output = Buffer_tools.Encode.float 0.0 in
  let expected = Bytes.of_string "\000\000\000\000\000\000\000\000" in
  check bytes "Float is 1.0" expected output

let () =
  run
    "float encoding"
    [
      ("float is 0.0", [test_case "0.0" `Quick test_float_zero]);
      ("float is 1.0", [test_case "1.0" `Quick test_float_one]);
    ]
