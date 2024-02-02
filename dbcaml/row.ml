(* Row.t will be used to create the row type we want to return to the users*)
type t = string option list

(*
* TODO: be able to take a type and map json data to the type
*)

let map_to (row : t) =
  List.fold_left
    (fun acc s ->
      match s with
      | Some v -> v :: acc
      | None -> acc)
    []
    row
