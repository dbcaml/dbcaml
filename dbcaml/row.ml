(* Row.t will be used to create the row type we want to return to the users*)
type t = string option list

let row_to_type (row : t) (t : 'to_type) =
  List.fold_left
    (fun acc s ->
      match s with
      | Some v -> v :: acc
      | None -> acc)
    []
    row

let rows_to_type (rows : t list) (t : 'to_type) = List.map row_to_type rows
