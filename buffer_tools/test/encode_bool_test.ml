open Alcotest

let test_true_bool () =
  let output = Buffer_tools.Encode.bool true in
  let expected = Bytes.of_string "\001" in
  check bytes "Bool is false" expected output

let test_false_bool () =
  let output = Buffer_tools.Encode.bool false in
  let expected = Bytes.of_string "\000" in
  check bytes "Bool is false" expected output

let () =
  run
    "bool encoding"
    [
      ("True bool", [test_case "Bool is true" `Quick test_true_bool]);
      ("False bool", [test_case "Bool is false" `Quick test_false_bool]);
    ]
