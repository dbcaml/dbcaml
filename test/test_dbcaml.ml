open OUnit2

let test_addition _ = assert_equal 3 3 ~printer:string_of_int

let suite = "simple_tests" >::: ["test_addition" >:: test_addition]

let () = run_test_tt_main suite
