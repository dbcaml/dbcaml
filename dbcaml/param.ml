type t = string

let params params =
  match params with
  | Some opts -> opts
  | None -> []
