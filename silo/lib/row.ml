(* Row.t will be used to create the row type we want to return to the users*)
type t = string list

(* Row.t will be used to create the row type we want to return to the users*)
(* This should be mapped to a record later on *)
let row_to_type (row : t) = row

let rows_to_type (rows : t list) = List.map row_to_type rows
