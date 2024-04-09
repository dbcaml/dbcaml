type oid = Oid of int

type field = {
  name: string;
  relation_id: int option;
  relation_attribute_no: int option;
  data_type_id: oid;
  data_type_size: int;
  type_modifier: int;
  format: int;
}

type row_description = { fields: field list }

let get_u16 buf =
  let value = Bytes.get_uint16_be buf 0 in
  let new_bytes = Bytes.sub buf 2 (Bytes.length buf - 2) in
  (value, new_bytes)

let index_of_backslash_zero bytes =
  let rec aux i =
    if i >= Bytes.length bytes - 1 then
      None
    else if Bytes.get bytes i = '\\' && Bytes.get bytes (i + 1) = '0' then
      Some i
    else
      aux (i + 1)
  in
  aux 0

let get_str_nul buf =
  let ending = index_of_backslash_zero buf |> Option.get in
  print_int ending;
  print_newline ();

  buf

let rec decode_row index amount_fields acc buf =
  if index < amount_fields then (
    Printf.printf "%d" index;
    let _name = get_str_nul buf in
    decode_row (index + 1) amount_fields acc buf
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
