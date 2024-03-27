(* module Row = Row *)
(* module Param = Param *)

(* let fetch_one pool_id ?params query = *)
(*   let result = *)
(*     match Dbcaml.execute pool_id params query with *)
(*     | Ok rows -> *)
(*       (match rows with *)
(*       | [] -> Error Dbcaml.Res.NoRows *)
(*       | r -> Ok (List.hd r)) *)
(*     | Error e -> Error e *)
(*   in *)

(*   result *)

(* let fetch_many pool_id ?params query = *)
(*   let result = *)
(*     match Dbcaml.execute pool_id params query with *)
(*     | Ok rows -> Ok rows *)
(*     | Error e -> Error e *)
(*   in *)

(*   result *)

(* let exec pool_id ?params query = *)
(*   let result = *)
(*     match Dbcaml.execute pool_id params query with *)
(*     | Ok _ -> Ok () *)
(*     | Error e -> Error e *)
(*   in *)

(*   result *)
