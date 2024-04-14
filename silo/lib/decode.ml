type field = {
  name: string;
  relation_id: int option;
  relation_attribute_no: int option;
  data_type_id: int;
  data_type_size: int;
  type_modifier: int;
  format: int;
}

let get_u16 buf =
  let value = Bytes.get_uint16_be buf 0 in
  let new_bytes = Bytes.sub buf 2 (Bytes.length buf - 2) in
  (value, new_bytes)

let get_i16 buf =
  let value = Bytes.get_int16_be buf 0 in
  let new_bytes = Bytes.sub buf 2 (Bytes.length buf - 2) in
  (value, new_bytes)

let get_i32 buf =
  let value = Bytes.get_int32_be buf 0 in
  let new_bytes = Bytes.sub buf 4 (Bytes.length buf - 4) in
  (value, new_bytes)

let get_u32 buf =
  let value = Bytes.get_int32_be buf 0 in
  let new_bytes = Bytes.sub buf 4 (Bytes.length buf - 4) in
  (value, new_bytes)

let index_of_null bs =
  let length = Bytes.length bs in
  let rec aux i =
    if i >= length then
      None
    else if Bytes.get bs i = '\000' then
      Some i
    else
      aux (i + 1)
  in
  aux 0

let get_str_nul buf =
  let ending = index_of_null buf |> Option.get in
  let value = Bytes.sub buf 0 ending |> Bytes.to_string in
  let new_bytes = Bytes.sub buf ending (Bytes.length buf - ending) in
  (value, new_bytes)

let rec decode_row index amount_fields (acc : field list) buf =
  if index < amount_fields then
    let (name, buf) = get_str_nul buf in
    let (relation_id, buf) =
      let (value, new_buf) = get_i32 buf in
      if Int32.compare value Int32.zero = 0 then
        (None, new_buf)
      else
        (Some (Int32.to_int value), new_buf)
    in
    let (relation_attribute_no, buf) =
      let (value, new_buf) = get_i16 buf in
      if value == 0 then
        (None, new_buf)
      else
        (Some value, new_buf)
    in
    let (data_type_id, buf) = get_u32 buf in
    let data_type_id = Int32.to_int data_type_id in
    let (data_type_size, buf) = get_i16 buf in
    let (type_modifier, buf) = get_i32 buf in
    let type_modifier = Int32.to_int type_modifier in
    let (format, buf) = get_i16 buf in

    let buf = Bytes.sub buf 1 (Bytes.length buf - 1) in

    let f =
      {
        name;
        relation_id;
        relation_attribute_no;
        data_type_id;
        data_type_size;
        type_modifier;
        format;
      }
    in
    (* TODO: return the new buffer without row description*)
    decode_row (index + 1) amount_fields (acc @ [f]) buf
  else
    (acc, buf)

let decode_row_description buf =
  let (amount_of_columns, buf) = get_u16 buf in

  let (fields, buf) = decode_row 0 amount_of_columns [] buf in

  Ok (buf, fields)
