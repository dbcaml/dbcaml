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

let encode_int32 n =
  let result = Bytes.create 4 in
  Bytes.set_int32_be result 0 n;
  result

let encode_int8 n = Bytes.make 1 (Char.chr n)

let serialize_param param =
  match param with
  | Dbcaml.Params.String str -> escape_sql_value str |> Bytes.of_string
  | Dbcaml.Params.Number i -> encode_int32 (Int32.of_int i)
  | Dbcaml.Params.Float f -> encode_int32 (Int32.of_float f)
  | Dbcaml.Params.Bool b ->
    if b then
      encode_int8 1
    else
      encode_int8 0
  | Dbcaml.Params.StringArray s ->
    let oid = Bytes.of_string "1015" in
    let length = List.length s in
    let header =
      Bytes.concat
        Bytes.empty
        [
          encode_int32 1l;
          encode_int32 0l;
          oid;
          encode_int32 (Int32.of_int length);
          encode_int32 1l;
        ]
    in
    let content =
      List.fold_left
        (fun acc x ->
          let escaped = escape_sql_value x in
          Bytes.cat
            acc
            (Bytes.concat
               Bytes.empty
               [
                 encode_int32 (String.length escaped |> Int32.of_int);
                 Bytes.of_string escaped;
               ]))
        Bytes.empty
        s
    in
    Bytes.concat Bytes.empty [header; content]
  | Dbcaml.Params.NumberArray s ->
    let oid = Bytes.of_string "1007" in
    let length = List.length s in
    let header =
      Bytes.concat
        Bytes.empty
        [
          encode_int32 (Int32.of_int 1);
          encode_int32 (Int32.of_int 0);
          oid;
          encode_int32 (Int32.of_int length);
          encode_int32 (Int32.of_int 1);
        ]
    in
    let content =
      List.fold_left
        (fun acc x ->
          Bytes.cat
            acc
            (Bytes.concat
               Bytes.empty
               [encode_int32 4l; encode_int32 (Int32.of_int x)]))
        Bytes.empty
        s
    in
    Bytes.concat Bytes.empty [header; content]

let encode_value value =
  let serialized_value = serialize_param value in
  let buffer = Buffer.create (Bytes.length serialized_value + 4) in

  let len =
    match Bytes.length serialized_value with
    | 0 -> -1
    | len -> len
  in

  Buffer.add_int32_be buffer (Int32.of_int len);
  Buffer.add_bytes buffer serialized_value;

  buffer

(* let encode_value value = *)
(*   let serialized_value = serialize_param value in *)
(*   let buffer = Buffer.create (Buffer.length serialized_value + 4) in *)
(**)
(*   Buffer.add_int32_be buffer (Int32.of_int (Buffer.length buffer)); *)
(*   Buffer.ad buffer serialized_value; *)
(**)
(*   buffer *)

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
