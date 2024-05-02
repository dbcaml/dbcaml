(** The available params a driver a driver need to support *)
type t =
  | String of string
  | Number of int
  | Float of float
  | Bool of bool
  | StringArray of string list
  | NumberArray of int list
