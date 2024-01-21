let encode_startup_message username database =
  let startup_message =
    "user\x00"
    ^ username
    ^ "\x00database\x00"
    ^ database
    ^ "\x00application_name\x00psql\x00"
  in

  let len = String.length startup_message + 4 in
  let length_prefix = String.make 4 '\000' in
  let length_bytes = Bytes.of_string length_prefix in
  Bytes.set length_bytes 0 (Char.chr (len lsr 24));
  Bytes.set length_bytes 1 (Char.chr ((len lsr 16) land 0xFF));
  Bytes.set length_bytes 2 (Char.chr ((len lsr 8) land 0xFF));
  Bytes.set length_bytes 3 (Char.chr (len land 0xFF));
  length_prefix ^ startup_message

let send_startup_message out_channel username database =
  let encoded_message = encode_startup_message username database in
  output_string out_channel encoded_message;
  flush out_channel;
  ()

let handle_auth_request out_channel in_channel password =
  let auth_msg = "\x00\x00\x00\x00" ^ password ^ "\x00" in
  output_string out_channel auth_msg;
  flush out_channel;

  let auth_resp = input_line in_channel in

  if String.contains auth_resp 'R' then (
    let password_msg = "\x00\x00\x00\x00" ^ password ^ "\x00" in
    output_string out_channel password_msg;
    flush out_channel
  )
