module Bs = Bytestring

let ( let* ) = Result.bind

let remove_null_bytes (bytes : bytes) : string =
  let str = Bytes.to_string bytes in
  String.concat "" (String.split_on_char '\000' str)

let authenticate conn message_format _ user password =
  match message_format with
  | Message_format.Authentication ->
    let* _ = Scram_auth.authenticate conn false user password in

    Ok conn
  | mf ->
    Error
      (`Msg
        (Printf.sprintf
           "Unexpected message format: %s"
           (Message_format.to_string mf)))
