open Alcotest

let test_samestring () = check string "same string" "hello" "hello"

let () =
  let open Alcotest in
  run
    "Temp tests"
    [("Same string", [test_case "Test is same string" `Quick test_samestring])]
