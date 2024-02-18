type t =
  | Number of int
  | String of string
  | Float of float
  | Bool of bool
  | Null
  | Array of t list
