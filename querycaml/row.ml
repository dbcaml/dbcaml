(* Row.t will be used to create the row type we want to return to the users*)
(* This should be mapped to a record later on *)
let row_to_type (row : Dbcaml.Row.t) = row

let rows_to_type (rows : Dbcaml.Row.t list) = List.map row_to_type rows
