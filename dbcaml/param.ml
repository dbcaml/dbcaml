type t =
  | String of string
  | Number of int
  | Bool of bool
  | Float of float
  | Null
  | Array of t list

let params params =
  match params with
  | Some opts -> opts
  | None -> []
