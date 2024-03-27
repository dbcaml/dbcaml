open Alcotest

let test_put_str_nul () =
  let buffer = Buffer.create 100 in

  let message = "Hello World" in

  let expected = message ^ "\000" in

  Buffer_tools.put_str_null buffer message;

  let result = Buffer.contents buffer in

  check string "same string" expected result

let () =
  Alcotest.run
    "Put str with null"
    [("Put str with null", [("put_str_nul", `Quick, test_put_str_nul)])]
