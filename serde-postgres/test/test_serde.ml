open Alcotest

(* type user = string [@@deriving deserialize] *)
(* let test_single_item () = *)
(*   let message = *)
(*     "T\000\000\000\192\000\007name\000\000\000@\005\000\002\000\000\004\019\255\255\000\000\001\003\000\000id\000\000\000@\005\000\001\000\000\000\023\000\004\255\255\255\255\000\000some_bool\000\000\000@\005\000\003\000\000\000\016\000\001\255\255\255\255\000\000pet_name\000\000\000@\005\000\007\000\000\004\019\255\255\000\000\001\003\000\000some_int64\000\000\000@\005\000\004\000\000\000\023\000\004\255\255\255\255\000\000some_int32\000\000\000@\005\000\005\000\000\000\023\000\004\255\255\255\255\000\000some_float\000\000\000@\005\000\006\000\000\002\189\000\b\255\255\255\255\000\000D\000\000\000.\000\007\000\000\000\005Alice\000\000\000\0011\000\000\000\001t\255\255\255\255\000\000\000\0011\000\000\000\0012\000\000\000\0031.1D\000\000\0001\000\007\000\000\000\003Bob\000\000\000\0012\000\000\000\001t\000\000\000\005Danza\000\000\000\0011\000\000\000\0012\000\000\000\0031.1C\000\000\000\rSELECT 2\000Z\000\000\000\005I" *)
(*     |> Bytes.of_string *)
(*   in *)
(*   let fetched_user = *)
(*     match Serde_postgres.of_rows deserialize_user message with *)
(*     | Ok u -> List.hd u *)
(*     | Error e -> fail (Format.asprintf "Deserialize error: %a" Serde.pp_err e) *)
(*   in *)
(*   Alcotest.(check int) *)
(*     "Does deserialize and have the expected value" *)
(*     49 *)
(*     fetched_user.id *)
type user = {
  name: string;
  id: int;
  some_int64: int64;
  some_int32: int32;
  some_float: float;
  some_bool: bool;
  pets: string list;
  pets_array: string array;
  pet_name: string option;
}
[@@deriving deserialize]

let test_record () =
  let message =
    "T\000\000\000\244\000\tname\000\000\000@\005\000\002\000\000\004\019\255\255\000\000\001\003\000\000id\000\000\000@\005\000\001\000\000\000\023\000\004\255\255\255\255\000\000some_bool\000\000\000@\005\000\003\000\000\000\016\000\001\255\255\255\255\000\000pet_name\000\000\000@\005\000\b\000\000\004\019\255\255\000\000\001\003\000\000some_int64\000\000\000@\005\000\004\000\000\000\020\000\b\255\255\255\255\000\000some_int32\000\000\000@\005\000\005\000\000\000\023\000\004\255\255\255\255\000\000some_float\000\000\000@\005\000\006\000\000\002\189\000\b\255\255\255\255\000\000pets\000\000\000@\005\000\007\000\000\003\241\255\255\255\255\255\255\000\000pets_array\000\000\000@\005\000\007\000\000\003\241\255\255\255\255\255\255\000\000D\000\000\000k\000\t\000\000\000\006Alice,\000\000\000\0011\000\000\000\001t\255\255\255\255\000\000\000\0011\000\000\000\0012\000\000\000\0031.1\000\000\000\026{Lucy,Adam,\"helllo,world\"}\000\000\000\026{Lucy,Adam,\"helllo,world\"}D\000\000\000O\000\t\000\000\000\003Bob\000\000\000\0012\000\000\000\001t\000\000\000\005Danza\000\000\000\0011\000\000\000\0012\000\000\000\0031.1\000\000\000\011{Lucy,Adam}\000\000\000\011{Lucy,Adam}D\000\000\000S\000\t\000\000\000\007Charlie\000\000\000\0013\000\000\000\001t\000\000\000\005Danza\000\000\000\0011\000\000\000\0012\000\000\000\0031.1\000\000\000\011{Lucy,Adam}\000\000\000\011{Lucy,Adam}D\000\000\000Q\000\t\000\000\000\005David\000\000\000\0014\000\000\000\001t\000\000\000\005Danza\000\000\000\0011\000\000\000\0012\000\000\000\0031.1\000\000\000\011{Lucy,Adam}\000\000\000\011{Lucy,Adam}D\000\000\000O\000\t\000\000\000\003Eve\000\000\000\0015\000\000\000\001t\000\000\000\005Danza\000\000\000\0011\000\000\000\0012\000\000\000\0031.1\000\000\000\011{Lucy,Adam}\000\000\000\011{Lucy,Adam}D\000\000\000Q\000\t\000\000\000\005Frank\000\000\000\0016\000\000\000\001t\000\000\000\005Danza\000\000\000\0011\000\000\000\0012\000\000\000\0031.1\000\000\000\011{Lucy,Adam}\000\000\000\011{Lucy,Adam}D\000\000\000Q\000\t\000\000\000\005Grace\000\000\000\0017\000\000\000\001t\000\000\000\005Danza\000\000\000\0011\000\000\000\0012\000\000\000\0031.1\000\000\000\011{Lucy,Adam}\000\000\000\011{Lucy,Adam}D\000\000\000R\000\t\000\000\000\006Hannah\000\000\000\0018\000\000\000\001t\000\000\000\005Danza\000\000\000\0011\000\000\000\0012\000\000\000\0031.1\000\000\000\011{Lucy,Adam}\000\000\000\011{Lucy,Adam}D\000\000\000O\000\t\000\000\000\003Ian\000\000\000\0019\000\000\000\001t\000\000\000\005Danza\000\000\000\0011\000\000\000\0012\000\000\000\0031.1\000\000\000\011{Lucy,Adam}\000\000\000\011{Lucy,Adam}D\000\000\000Q\000\t\000\000\000\004Jane\000\000\000\00210\000\000\000\001t\000\000\000\005Danza\000\000\000\0011\000\000\000\0012\000\000\000\0031.1\000\000\000\011{Lucy,Adam}\000\000\000\011{Lucy,Adam}C\000\000\000\014SELECT 10\000Z\000\000\000\005I"
    |> Bytes.of_string
  in

  let fetched_user =
    match Serde_postgres.of_bytes deserialize_user message with
    | Ok u -> u
    | Error e -> fail (Format.asprintf "Deserialize error: %a" Serde.pp_err e)
  in

  Alcotest.(check string)
    "Does deserialize and have the expected value"
    fetched_user.name
    "Alice,";

  Alcotest.(check bool)
    "Does deserialize and have the expected value"
    fetched_user.some_bool
    true;

  Alcotest.(check (option string))
    "Does deserialize options"
    fetched_user.pet_name
    None;

  Alcotest.(check (float 1e-2))
    "Does deserialize and have the expected value"
    fetched_user.some_float
    1.1;

  Alcotest.(check int32)
    "Does deserialize and have the expected value"
    (Int32.of_int 2)
    fetched_user.some_int32;

  Alcotest.(check int64)
    "Does deserialize and have the expected value"
    (Int64.of_int 1)
    fetched_user.some_int64;

  Alcotest.(check int)
    "Does deserialize and have the expected value"
    1
    fetched_user.id;

  Alcotest.(check int)
    "Does deserialize and have the expected value"
    3
    (Array.length fetched_user.pets_array);

  Alcotest.(check int)
    "Does deserialize and have the expected value"
    3
    (List.length fetched_user.pets)

(* type user_list = user list [@@deriving deserialize] *)
(* let test_record_list () = *)
(*   let message = *)
(*     "T\000\000\000\192\000\007name\000\000\000@\005\000\002\000\000\004\019\255\255\000\000\001\003\000\000id\000\000\000@\005\000\001\000\000\000\023\000\004\255\255\255\255\000\000some_bool\000\000\000@\005\000\003\000\000\000\016\000\001\255\255\255\255\000\000pet_name\000\000\000@\005\000\007\000\000\004\019\255\255\000\000\001\003\000\000some_int64\000\000\000@\005\000\004\000\000\000\020\000\b\255\255\255\255\000\000some_int32\000\000\000@\005\000\005\000\000\000\023\000\004\255\255\255\255\000\000some_float\000\000\000@\005\000\006\000\000\002\189\000\b\255\255\255\255\000\000D\000\000\000.\000\007\000\000\000\005Alice\000\000\000\0011\000\000\000\001t\255\255\255\255\000\000\000\0011\000\000\000\0012\000\000\000\0031.1D\000\000\0001\000\007\000\000\000\003Bob\000\000\000\0012\000\000\000\001t\000\000\000\005Danza\000\000\000\0011\000\000\000\0012\000\000\000\0031.1D\000\000\0005\000\007\000\000\000\007Charlie\000\000\000\0013\000\000\000\001t\000\000\000\005Danza\000\000\000\0011\000\000\000\0012\000\000\000\0031.1D\000\000\0003\000\007\000\000\000\005David\000\000\000\0014\000\000\000\001t\000\000\000\005Danza\000\000\000\0011\000\000\000\0012\000\000\000\0031.1D\000\000\0001\000\007\000\000\000\003Eve\000\000\000\0015\000\000\000\001t\000\000\000\005Danza\000\000\000\0011\000\000\000\0012\000\000\000\0031.1D\000\000\0003\000\007\000\000\000\005Frank\000\000\000\0016\000\000\000\001t\000\000\000\005Danza\000\000\000\0011\000\000\000\0012\000\000\000\0031.1D\000\000\0003\000\007\000\000\000\005Grace\000\000\000\0017\000\000\000\001t\000\000\000\005Danza\000\000\000\0011\000\000\000\0012\000\000\000\0031.1D\000\000\0004\000\007\000\000\000\006Hannah\000\000\000\0018\000\000\000\001t\000\000\000\005Danza\000\000\000\0011\000\000\000\0012\000\000\000\0031.1D\000\000\0001\000\007\000\000\000\003Ian\000\000\000\0019\000\000\000\001t\000\000\000\005Danza\000\000\000\0011\000\000\000\0012\000\000\000\0031.1D\000\000\0003\000\007\000\000\000\004Jane\000\000\000\00210\000\000\000\001t\000\000\000\005Danza\000\000\000\0011\000\000\000\0012\000\000\000\0031.1C\000\000\000\014SELECT 10\000Z\000\000\000\005I" *)
(*     |> Bytes.of_string *)
(*   in *)
(*   let users = *)
(*     match Serde_postgres.of_bytes deserialize_user_list message with *)
(*     | Ok u -> List.hd u *)
(*     | Error e -> fail (Format.asprintf "Deserialize error: %a" Serde.pp_err e) *)
(*   in *)
(**)
(*   Alcotest.(check int) *)
(*     "Did deserialize the right amount of users" *)
(*     3 *)
(*     (List.length users) *)

let () =
  let open Alcotest in
  run
    "Deserialize"
    [
      ("Single record", [test_case "single_record" `Quick test_record]);
      (* ("Single item", [test_case "single_item" `Quick test_single_item]); *)
      (* ("List of records", [test_case "single_record" `Quick test_record_list]); *)
    ]
