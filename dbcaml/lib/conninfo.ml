let ( let* ) = Option.bind

type t =
  | ConnectionInformation : {
      host: string;
      port: int;
      username: string;
      password: string;
      schema: string;
      database: string;
    }
      -> t

let remove_leading_slash initial_string =
  if String.length initial_string > 0 && String.get initial_string 0 = '/' then
    String.sub initial_string 1 (String.length initial_string - 1)
  else
    initial_string

let of_string connection_info =
  let parsed_url = Uri.of_string connection_info in
  let* host = Uri.host parsed_url in
  let* port = Uri.port parsed_url in
  let* username = Uri.user parsed_url in
  let* password = Uri.password parsed_url in
  let database = Uri.path parsed_url |> remove_leading_slash in

  Some
    (ConnectionInformation
       { host; port; username; password; schema = "postgresql"; database })
