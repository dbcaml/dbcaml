let escape_sql_value value =
  let buffer = Buffer.create (String.length value) in
  String.iter
    (fun c ->
      match c with
      | '\'' ->
        Buffer.add_string
          buffer
          "''" (* Replace single quote with two single quotes *)
      | _ -> Buffer.add_char buffer c)
    value;
  Buffer.contents buffer

let serialize_param param =
  let value =
    match param with
    | Dbcaml.Param.String str -> str
    | Dbcaml.Param.Number i ->
      let buffer = Buffer.create 4 in
      Buffer.add_int32_be buffer (Int32.of_int i);
      Buffer.contents buffer
    | Dbcaml.Param.Float f -> string_of_float f
    | Dbcaml.Param.Bool b -> string_of_bool b
  in

  escape_sql_value value

let encode_value value =
  let seriaized_value = serialize_param value in
  let buffer = Buffer.create (String.length seriaized_value + 4) in

  let encoded_value = Bytes.of_string seriaized_value in

  let len =
    match Bytes.length encoded_value with
    | 0 -> -1
    | len -> len
  in

  Buffer.add_int32_be buffer (Int32.of_int len);
  Buffer.add_bytes buffer encoded_value;

  buffer
