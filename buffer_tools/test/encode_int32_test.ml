open Alcotest

let check key =
  check bytes (Printf.sprintf "same bytes for %s" (String.of_bytes key)) key

let test_positive_numbers () =
  check
    (Bytes.of_string "\000\000\000\001")
    (Buffer_tools.Encode.int32 (Int32.of_int 1));
  check
    (Bytes.of_string "\000\000\001\002")
    (Buffer_tools.Encode.int32 (Int32.of_int 258));
  check
    (Bytes.of_string "\000\001\001\001")
    (Buffer_tools.Encode.int32 (Int32.of_int 65793));
  check
    (Bytes.of_string "\001\002\003\004")
    (Buffer_tools.Encode.int32 (Int32.of_int 16909060))

let test_negative_numbers () =
  check
    (Bytes.of_string "\255\255\255\255")
    (Buffer_tools.Encode.int32 (Int32.neg (Int32.of_int 1)));
  check
    (Bytes.of_string "\254\253\252\252")
    (Buffer_tools.Encode.int32 (Int32.neg (Int32.of_int 16909060)))

let test_boundary_values () =
  check (Bytes.of_string "\128\000\000\000") (Buffer_tools.Encode.int32 Int32.min_int);
  check (Bytes.of_string "\127\255\255\255") (Buffer_tools.Encode.int32 Int32.max_int)

let test_zero () =
  check (Bytes.of_string "\000\000\000\000") (Buffer_tools.Encode.int32 Int32.zero)

let () =
  run
    "int32 encoding"
    [
      ( "Positive numbers",
        [test_case "Buffer_tools.Encode positive numbers" `Quick test_positive_numbers]
      );
      ( "Negative numbers",
        [test_case "Buffer_tools.Encode negative numbers" `Quick test_negative_numbers]
      );
      ( "Boundary values",
        [test_case "Buffer_tools.Encode boundary values" `Quick test_boundary_values] );
      ("Zero", [test_case "Buffer_tools.Encode zero" `Quick test_zero]);
    ]
