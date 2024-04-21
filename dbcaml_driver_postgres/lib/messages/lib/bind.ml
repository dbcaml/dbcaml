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

(** Create a bind message with the statement_id, portal_name and params *)
let bind ~statement_id ~params ~portal_name =
  let bind_buffer = Buffer.create 0 in

  (* We're both sending and recieving data as binary *)
  let formats = [1] in

  Buffer.add_char bind_buffer 'B';
  Buffer_tools.put_length_prefixed bind_buffer (fun buf ->
      (* Add the portal_name and a null terminator *)
      Buffer.add_string buf portal_name;
      Buffer.add_int8 buf 0;

      (* Add the statement id and a null terminator *)
      Buffer.add_string buf statement_id;
      Buffer.add_int8 buf 0;

      (* Add the amount formats and each format. A format is either text(0) or binary(1) *)
      Buffer.add_int16_be buf (List.length formats);
      List.iter (fun format -> Buffer.add_int16_be buf format) formats;

      (* Add the params length and all the values *)
      Buffer.add_int16_be buf (List.length params);
      List.iter
        (fun param -> encode_value param |> Buffer.add_buffer buf)
        params;

      (* Add result formats *)
      Buffer.add_int16_be buf (List.length formats);
      List.iter (fun format -> Buffer.add_int16_be buf format) formats);

  bind_buffer
