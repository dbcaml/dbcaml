open Dbcaml.Param

let oid_of_type = function
  | String _ -> 1043 (* text *)
  | Number _ -> 23 (* integer *)
  | Float _ -> 700 (* real *)
  | Bool _ -> 16 (* boolean *)
