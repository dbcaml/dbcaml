let ( let* ) = Result.bind

type authentication_information = {
  user: string;
  password: string;
  database: string;
  host: string;
  port: int;
}

let remove_first_letter_if_slash str =
  if String.starts_with ~prefix:"/" str then
    String.sub str 1 (String.length str - 1)
  else
    str

let make ~conninfo =
  let uri = Uri.of_string conninfo in
  let user = Uri.user uri |> Option.value ~default:"" in
  let password = Uri.password uri |> Option.value ~default:"" in
  let database = Uri.path uri |> remove_first_letter_if_slash in
  let host = Uri.host uri |> Option.value ~default:"localhost" in
  let port = Uri.port uri |> Option.value ~default:5432 in

  { user; password; database; host; port }
