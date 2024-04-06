let put_length_prefixed buffer f =
  let new_buffer = Buffer.create 1 in

  f new_buffer;

  let message_length =
    (* the +4 is to add the size for the message length*)
    Buffer.length new_buffer + 4 |> Int32.of_int
  in

  Buffer.add_int32_be buffer message_length;
  Buffer.add_buffer buffer new_buffer

let put_str_null buffer str =
  Buffer.add_string buffer str;
  Buffer.add_char buffer '\000'
