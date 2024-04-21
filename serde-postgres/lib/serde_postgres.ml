open Serde

let ( let* ) = Result.bind

let rec string_of_char_list clist =
  match clist with
  | [] -> ""
  | head :: tail -> Char.escaped head ^ string_of_char_list tail

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

    let read_char s =
      let value = Bytes.get s.value s.value_pos in
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
      s.value_pos <- s.value_pos + length;
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

  let nest { reader; _ } =
    {
      reader;
      kind = First;
      in_sequence = false;
      column_names = [];
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
          (Parser.element_item_length
             ~escaped_string:state.is_escaped_string
             state.reader)
      | false -> Parser.read_column_length state.reader
    in
    Parser.read_string state.reader ~length

  let deserialize_option self s de =
    let length = Parser.get_length s.reader |> Int32.to_int in
    match length with
    | -1 ->
      let* _ = Parser.read_null s.reader in
      Ok None
    | _ ->
      let* v = De.deserialize self de in
      Ok (Some v)

  let deserialize_identifier self _state visitor =
    let* str = De.deserialize_string self in
    Visitor.visit_string self visitor str

  let deserialize_sequence self s ~size de =
    let* _ =
      match Parser.read_row_description s.reader with
      | Ok (Some column_names) ->
        s.column_names <- column_names;
        Ok ()
      | Ok None -> Ok ()
      | _ -> failwith "unexpected eof"
    in
    let* rows = De.deserialize self (de ~size) in
    Ok rows

  let deserialize_element self s de =
    match Parser.peek s.reader with
    | Some '}' ->
      s.in_list <- false;
      s.kind <- First;
      Ok None
    | Some peek ->
      let* () =
        if s.kind = First then
          let* _ = Parser.read_list_open s.reader in
          Ok ()
        else
          Ok ()
      in
      s.in_list <- true;
      s.kind <- Rest;
      s.is_escaped_string <- peek = '\"';
      let* v = De.deserialize self de in
      let* _ = Parser.read_comma s.reader in
      Ok (Some v)
    | None -> failwith "unexpected value"

  let deserialize_unit_variant _self _state = Ok ()

  let deserialize_newtype_variant self _ de = De.deserialize self de

  let deserialize_tuple_variant self _state ~size de =
    De.deserialize_sequence self size de

  let deserialize_record_variant self _state ~size de =
    De.deserialize_record self "" size (de ~size)

  let deserialize_variant _self { reader; _ } _de ~name:_ ~variants:_ =
    match Parser.peek reader with
    | _ -> assert false

  let deserialize_record self s ~name:_ ~size:_ fields =
    let* _ =
      match Parser.read_row_description s.reader with
      | Ok (Some column_names) ->
        s.column_names <- column_names;
        Ok ()
      | Ok None -> Ok ()
      | _ -> failwith "unexpected eof"
    in
    match Parser.peek s.reader with
    | Some 'D' ->
      s.kind <- First;
      let* _ = Parser.read_int32_be ~length:4 s.reader in
      let* _ = Parser.read_int16_be ~length:2 s.reader in
      let* value = De.deserialize self fields in
      Ok value
    | Some c -> failwith (Printf.sprintf "what: %C" c)
    | None -> failwith "unexpected eof"

  let deserialize_key self state visitor =
    match state.current_column_idx < List.length state.column_names with
    | true ->
      let key = List.nth state.column_names state.current_column_idx in
      state.current_column_idx <- state.current_column_idx + 1;
      let* key = Visitor.visit_string self visitor key in
      Ok (Some key)
    | false -> Ok None

  let deserialize_field self _s ~name:_ de = De.deserialize self de

  let deserialize_ignored_any _self s =
    match Parser.peek s.reader with
    | Some _ ->
      let* _ = Parser.read_int32 ~length:4 s.reader in
      Ok ()
    | None -> failwith "unexpected eof"
end

let of_bytes de buf =
  let state =
    Deserializer.
      {
        reader = Postgres.Parser.of_bytes buf;
        kind = First;
        in_sequence = false;
        column_names = [];
        current_column_idx = 0;
        in_list = false;
        list_length = 0;
        is_escaped_string = false;
      }
  in

  Serde.deserialize (module Deserializer) state de
