(* Row.t will be used to create the row type we want to return to the users*)
type t = string option list

let row_to_type (row : t) =
  List.fold_left
    (fun acc s ->
      match s with
      | Some v -> v :: acc
      | None -> acc)
    []
    row

let rows_to_type (rows : t list) = List.map row_to_type rows
