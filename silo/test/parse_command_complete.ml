open Alcotest

let test_parse_command_complete () =
  (* Create a testable for the Result type using predefined testables for `int` and `string`. *)
  let result_testable = Alcotest.(result int string) in
  let check = Alcotest.check result_testable "parse_command_complete" in
  let tests =
    [
      ("C\000\000\000\015INSERT 0 1\000", Ok 1);
      ("C\000\000\000\rUPDATE 9\000", Ok 9);
      ("C\000\000\000\rDELETE 9\000", Ok 9);
      ( "C\000\000\000\015INSERT invalid\000",
        Error "failed to parse command complete message" );
      ( "C\000\000\000\rINVALID INPUT",
        Error "failed to parse command complete message" );
    ]
  in
  List.iter
    (fun (input, expected) ->
      check (Silo.parse_command_complete input) expected)
    tests

let suite = [test_case "Test cases" `Quick test_parse_command_complete]
