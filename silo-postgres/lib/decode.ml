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
  if index < amount_fields then (
    let (name, buf) = get_str_nul buf in
    Printf.printf "name: %s \n" name;
    let (relation_id, buf) =
      let (value, new_buf) = get_i32 buf in
      if Int32.compare value Int32.zero = 0 then
        (None, new_buf)
      else
        (Some (Int32.to_int value), new_buf)
    in
    Printf.printf "relation_id: %d \n" (Option.get relation_id);
    let (relation_attribute_no, buf) =
      let (value, new_buf) = get_i16 buf in
      if value == 0 then
        (None, new_buf)
      else
        (Some value, new_buf)
    in
    Printf.printf
      "relation_attribute_no: %d \n"
      (Option.get relation_attribute_no);
    let (data_type_id, buf) = get_u32 buf in
    let data_type_id = Int32.to_int data_type_id in
    Printf.printf "data_type_id: %d \n" data_type_id;
    let (data_type_size, buf) = get_i16 buf in
    Printf.printf "data_type_size: %d \n" data_type_size;
    let (type_modifier, buf) = get_i32 buf in
    let type_modifier = Int32.to_int type_modifier in
    Printf.printf "type_modifier: %d \n" type_modifier;
    let (format, buf) = get_i16 buf in
    Printf.printf "format: %d \n" format;

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
  ) else
    acc

let decode_row_description buf : (field list, string) result =
  Printf.printf "%S\n\n" (Bytes.to_string buf);
  let (cnt, buf) = get_u16 buf in
  print_newline ();
  print_newline ();
  Printf.printf "Value: %d" cnt;
  Printf.printf "%S" (String.of_bytes buf);
  print_newline ();
  print_newline ();

  let fields = decode_row 0 cnt [] buf in

  Ok fields
