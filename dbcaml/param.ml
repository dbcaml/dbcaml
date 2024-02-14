type t =
  | String of string
  | Number of int
  | Bool of bool
  | Float of float

let params params =
  match params with
  | Some opts -> opts
  | None -> []
