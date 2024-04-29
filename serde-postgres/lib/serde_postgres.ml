open Serde

let ( let* ) = Result.bind

let rec string_of_char_list clist =
  match clist with
  | [] -> ""
  | head :: tail -> Char.escaped head ^ string_of_char_list tail

(* Function to find the end of an item in a comma-separated list with potential quoted and escaped parts *)
let find_end_of_item s =
  let s_len = String.length s in

  (* Finds the end of the current item, considering escaping and quoted strings *)
  let rec aux pos inside_quotes =
    if pos >= s_len then
      pos
    (* If reached end of string, return position *)
    else
      match s.[pos] with
      | '}' when not inside_quotes ->
        pos (* Include '}' as part of the structure end *)
      | '"' -> aux (pos + 1) (not inside_quotes) (* Toggle quote state *)
      | '\\' when pos + 1 < s_len ->
        aux pos inside_quotes (* Skip escaped character *)
      | ',' when not inside_quotes ->
        pos (* Return position at comma, end of item *)
      | _ -> aux (pos + 1) inside_quotes
  in
  aux 0 false

let unescape_string s =
  let len = String.length s in
  if len >= 2 && String.get s 0 = '\"' && String.get s (len - 1) = '\"' then
    String.sub s 1 (len - 2)
  else
    s

module Postgres = struct
  module Parser = struct
    type t = {
      value: bytes;
      mutable value_pos: int;
    }

    let get_current_buf { value; value_pos } =
      Bytes.sub value value_pos (Bytes.length value - value_pos)

    let debug t = Printf.printf "buff: %S" (Bytes.to_string t)

    let of_bytes value = { value; value_pos = 0 }

    let _run fn = Ok (fn ())

    let sub_string s ~length =
      let value = Bytes.sub_string s.value s.value_pos length in
      s.value_pos <- s.value_pos + length;
      value

    let read_int s ~length = Ok (sub_string s ~length |> int_of_string)

    let read_int8 s ~length =
      Ok (sub_string s ~length |> int_of_string |> char_of_int)

    let read_int31 s ~length = Ok (sub_string s ~length |> int_of_string)

    let read_int32 s ~length = Ok (sub_string s ~length |> Int32.of_string)

    let read_int64 s ~length = Ok (sub_string s ~length |> Int64.of_string)

    let peek s =
      let value = Bytes.get s.value s.value_pos in
      (* Increase position with 1 as we only want to read 1 byte *)
      s.value_pos <- s.value_pos + 1;
      Some value

    (* Get the column length without increasing position *)
    let get_length { value; value_pos } = Bytes.get_int32_be value value_pos

    (* Read the column length and increase the bytes with 4 *)
    let read_column_length s =
      let value = get_length s |> Int32.to_int in
      s.value_pos <- s.value_pos + 4;
      Ok value

    (* Function used when reading RowDescription to get how many further the end of the column name is *)
    (* RowDescription don't include this as DataRow does  *)
    let index_of state char_to_find =
      let rec aux i =
        if Bytes.get state.value (state.value_pos + i) = char_to_find then
          Some i
        else
          aux (i + 1)
      in
      aux 0

    (* Function to get a null-terminated string from a lexbuf and update its position *)
    let get_str_nul s =
      match index_of s '\000' with
      | Some ending ->
        let value = Bytes.sub s.value s.value_pos ending |> Bytes.to_string in
        s.value_pos <- s.value_pos + ending + 1;
        Ok value
      | None -> Error "Null terminator not found"

    let read_bool s ~length =
      let value = Bytes.get s.value s.value_pos in
      s.value_pos <- s.value_pos + length;
      match value with
      | 't' -> Ok true
      | 'f' -> Ok false
      | _ -> Error (`Msg "can't read bool")

    let read_int8_be s ~length =
      let value = Bytes.get_int8 s.value s.value_pos in
      s.value_pos <- s.value_pos + length;
      Ok (char_of_int value)

    let read_int16_be s ~length =
      let value = Bytes.get_int16_be s.value s.value_pos in
      (* Increase position with 2 as we read 2 bytes *)
      s.value_pos <- s.value_pos + length;
      Ok value

    let read_int_be s ~length =
      let value = Bytes.get_int8 s.value s.value_pos in
      (* Increase position with 1 as we read 1 bytes *)
      s.value_pos <- s.value_pos + length;
      Ok value

    let read_int32_be s ~length =
      let value = Bytes.get_int32_be s.value s.value_pos in
      (* Increase position with 4 as we read 4 bytes *)
      s.value_pos <- s.value_pos + length;
      Ok value

    let read_int64_be s ~length =
      let value = Bytes.get_int64_be s.value s.value_pos in
      (* Increase position with 8 as we read 8 bytes *)
      s.value_pos <- s.value_pos + length;
      Ok value

    let read_char_no_move s = Bytes.get s.value s.value_pos

    let read_next_char_no_move s = Bytes.get s.value (s.value_pos + 1)

    let read_char s =
      let value = read_char_no_move s in
      (* Increase position with 1 as we read 1 bytes*)
      s.value_pos <- s.value_pos + 1;
      value

    let read_string s ~length =
      let value =
        List.init length (fun _ -> read_char s) |> string_of_char_list
      in
      Ok value

    let read_float s ~length =
      let value =
        List.init length (fun _ -> read_char s)
        |> string_of_char_list
        |> float_of_string
      in
      Ok value

    (* Row values is just 4 bytes with the value of -1. If we get a NULL value do we simply offset the pos with 4 *)
    let read_null s = _run (fun () -> s.value_pos <- s.value_pos + 4)

    let read_list_open s = _run (fun () -> s.value_pos <- s.value_pos + 1)

    let read_comma s =
      _run (fun () ->
          match Bytes.get s.value s.value_pos with
          | ',' -> s.value_pos <- s.value_pos + 1
          | _ -> ())

    let read_row_description s =
      let value = Bytes.get s.value s.value_pos in
      match value with
      | 'T' ->
        (* Skip 5 first bytes as it's just message_type and total length of the message*)
        s.value_pos <- s.value_pos + 5;
        let* header_fields_count = read_int16_be ~length:2 s in
        Ok
          (Some
             (List.init header_fields_count (fun _ ->
                  let column_name = get_str_nul s |> Result.get_ok in
                  let _ = read_int32_be s ~length:4 |> Result.get_ok in
                  let _ = read_int16_be s ~length:2 |> Result.get_ok in
                  let _ = read_int32_be s ~length:4 |> Result.get_ok in
                  let _ = read_int16_be s ~length:2 |> Result.get_ok in
                  let _ = read_int32_be s ~length:4 |> Result.get_ok in
                  let _ = read_int16_be s ~length:2 |> Result.get_ok in
                  column_name)))
      | _ -> Ok None

    let element_item_length ?(escaped_string = false) s =
      match escaped_string with
      | true ->
        (match index_of s '\"' with
        | Some i -> i + 1
        | None -> 0)
      | false ->
        (match index_of s ',' with
        | Some i -> i
        | None -> 0)
  end
end

module Deserializer = struct
  open Postgres

  type kind =
    | First
    | Rest

  type state = {
    reader: Parser.t;
    mutable kind: kind;
    mutable in_sequence: bool;
    mutable column_names: string list;
    mutable current_column_idx: int;
    mutable in_list: bool;
    mutable list_length: int;
    mutable is_escaped_string: bool;
  }

  let nest { reader; column_names; _ } =
    {
      reader;
      kind = First;
      in_sequence = false;
      column_names;
      current_column_idx = 0;
      in_list = false;
      list_length = 0;
      is_escaped_string = false;
    }

  let deserialize_int8 _self state =
    let* length =
      match state.in_list with
      | true -> Ok (Parser.element_item_length state.reader)
      | false -> Parser.read_column_length state.reader
    in
    Parser.read_int8 state.reader ~length

  let deserialize_int16 _self state =
    let* length =
      match state.in_list with
      | true -> Ok (Parser.element_item_length state.reader)
      | false -> Parser.read_column_length state.reader
    in
    Parser.read_int state.reader ~length

  let deserialize_int31 _self state =
    let* length =
      match state.in_list with
      | true -> Ok (Parser.element_item_length state.reader)
      | false -> Parser.read_column_length state.reader
    in
    Parser.read_int state.reader ~length

  let deserialize_int32 _self state =
    let* length =
      match state.in_list with
      | true -> Ok (Parser.element_item_length state.reader)
      | false -> Parser.read_column_length state.reader
    in
    Parser.read_int32 state.reader ~length

  let deserialize_int64 _self state =
    let* length =
      match state.in_list with
      | true -> Ok (Parser.element_item_length state.reader)
      | false -> Parser.read_column_length state.reader
    in
    Parser.read_int64 state.reader ~length

  let deserialize_float _self state =
    let* length =
      match state.in_list with
      | true -> Ok (Parser.element_item_length state.reader)
      | false -> Parser.read_column_length state.reader
    in
    Parser.read_float state.reader ~length

  let deserialize_bool _self state =
    let* length =
      match state.in_list with
      | true -> Ok (Parser.element_item_length state.reader)
      | false -> Parser.read_column_length state.reader
    in
    Parser.read_bool state.reader ~length

  let deserialize_string _self state =
    let* length =
      match state.in_list with
      | true ->
        Ok
          (find_end_of_item
             (Parser.get_current_buf state.reader |> Bytes.to_string))
      | false -> Parser.read_column_length state.reader
    in
    let* value = Parser.read_string state.reader ~length in

    Ok (unescape_string value)

  let deserialize_option self state de =
    let length = Parser.get_length state.reader |> Int32.to_int in
    match length with
    | -1 ->
      let* _ = Parser.read_null state.reader in
      Ok None
    | _ ->
      let* v = De.deserialize self de in
      Ok (Some v)

  let deserialize_identifier self _state visitor =
    let* str = De.deserialize_string self in
    Visitor.visit_string self visitor str

  let deserialize_sequence self state ~size de =
    (match Parser.read_char_no_move state.reader with
    (* If we begin with reading a DataRow do we know that we are parsing list of rercords. Otherwise we are not parsing a list of records *)
    | 'D' -> state.in_sequence <- true
    | _ -> ());
    let* rows = De.deserialize self (de ~size) in
    Ok rows

  let deserialize_element self state de =
    match
      (Parser.read_char_no_move state.reader, state.in_sequence, state.in_list)
    with
    (* This match case handles the beginning of a new sequence, aka when we are parsing elements and not just list of strings *)
    | ('}', _, true) ->
      let _ = Parser.peek state.reader in
      state.in_list <- false;
      state.kind <- First;
      Ok None
    | ('}', true, false) ->
      Error (`Msg "we shouldn't read end for array and be in sequence mode")
    | ('D', true, false) ->
      (* Set column index to 0 everytime we start a new DataRow as we are now parsing a new row and it would be a index offset unless we reset the current column index  *)
      state.current_column_idx <- 0;
      let* v = De.deserialize self de in
      Ok (Some v)
      (* we dont have more rows to deserialize so we close. C = Command Complete *)
    | ('C', true, false) ->
      Ok None
      (* Handle if we are in list mode but we are not parsing the first item *)
    | (_, _, true) ->
      state.in_list <- true;
      state.kind <- Rest;
      let* v = De.deserialize self de in
      let* _ = Parser.read_comma state.reader in
      Ok (Some v)
    | (_, _, false) ->
      let* _ = Parser.read_int32_be state.reader ~length:4 in
      let* _ = Parser.read_list_open state.reader in
      state.in_list <- true;
      state.kind <- Rest;
      let* v = De.deserialize self de in
      let* _ = Parser.read_comma state.reader in
      Ok (Some v)

  let deserialize_unit_variant _self _state =
    Error (`Msg "variants deserialize not supported")

  let deserialize_newtype_variant _self _state _de =
    Error (`Msg "variants deserialize not supported")

  let deserialize_tuple_variant _self _state ~size:_ _de =
    Error (`Msg "variants deserialize not supported")

  let deserialize_record_variant _self _state ~size:_ _de =
    Error (`Msg "variants deserialize not supported")

  let deserialize_variant _self _state _de ~name:_ ~variants:_ =
    Error (`Msg "variants deserialize not supported")

  let deserialize_record self state ~name:_ ~size:_ fields =
    match (Parser.peek state.reader, state.in_sequence) with
    | (Some 'D', false) ->
      state.kind <- First;
      let* _ = Parser.read_int32_be ~length:4 state.reader in
      let* _ = Parser.read_int16_be ~length:2 state.reader in
      let* value = De.deserialize self fields in
      Ok value
    | (Some c, false) -> failwith (Printf.sprintf "what: %C" c)
    | (_, _) -> failwith "unexpected eof"

  let deserialize_key self state visitor =
    match state.current_column_idx < List.length state.column_names with
    | true ->
      let key = List.nth state.column_names state.current_column_idx in
      state.current_column_idx <- state.current_column_idx + 1;
      let* key = Visitor.visit_string self visitor key in
      Ok (Some key)
    | false -> Ok None

  let deserialize_field self _state ~name:_ de = De.deserialize self de

  let deserialize_ignored_any _self s =
    match Parser.peek s.reader with
    | Some _ ->
      let* _ = Parser.read_int32 ~length:4 s.reader in
      Ok ()
    | None -> failwith "unexpected eof"
end

(* TODO: improve this comment *)

(** Deserialize bytes from Postgres. of_bytes needs that the bytes starts with 'T' (DataRowDescription) field in order to operate as normal. 
    The DataRowDescription field is used by the deserializer to know which fields that exist in what order. 
    This means that the record could start with field id and name as order but the message from postgres don't need to be in that order.   *)
let of_bytes de buf =
  let* (buf, columns) = Row_description.decode_row_description buf in
  let column_names =
    List.map (fun (x : Row_description.field) -> x.name) columns
  in

  let state =
    Deserializer.
      {
        reader = Postgres.Parser.of_bytes buf;
        kind = First;
        in_sequence = false;
        column_names;
        current_column_idx = 0;
        in_list = false;
        list_length = 0;
        is_escaped_string = false;
      }
  in

  Serde.deserialize (module Deserializer) state de
