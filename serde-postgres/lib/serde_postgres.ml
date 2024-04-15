open Serde

let ( let* ) = Result.bind

module Postgres = struct
  module Parser = struct
    type t = { value: bytes }

    let debug t = Printf.printf "buff: %S" t

    let of_bytes value = { value }

    let _run fn = Ok (fn ())

    let peek _ = None (*FIXME: figure out what this is*)

    let read_bool { value } =
      _run (fun () -> bool_of_string (String.of_bytes value))

    let read_string { value } = _run (fun () -> String.of_bytes value)

    let read_int8 { value } =
      _run (fun () -> int_of_string (String.of_bytes value) |> char_of_int)

    let read_int { value } =
      _run (fun () -> int_of_string (String.of_bytes value))

    let read_int32 { value } =
      _run (fun () -> String.of_bytes value |> Int32.of_string)

    let read_int64 { value } =
      _run (fun () -> String.of_bytes value |> Int64.of_string)

    let read_float { value } =
      _run (fun () -> float_of_string (String.of_bytes value))

    let read_null _ = _run (fun () -> ()) (* FIXME: change this *)

    let read_object_start _ = _run (fun () -> ()) (* FIXME: change this *)

    let read_object_end _ = _run (fun () -> ()) (* FIXME: change this *)

    let skip_row_information _ = () (* FIXME: change this *)

    let read_end _ = _run (fun () -> ()) (* FIXME: change this *)
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
  }

  let nest { reader; _ } = { reader; kind = First }

  let deserialize_int8 _self state = Parser.read_int8 state.reader

  let deserialize_int16 _self state = Parser.read_int state.reader

  let deserialize_int31 _self state = Parser.read_int state.reader

  let deserialize_int32 _self state = Parser.read_int32 state.reader

  let deserialize_int64 _self state = Parser.read_int64 state.reader

  let deserialize_float _self state = Parser.read_float state.reader

  let deserialize_bool _self state = Parser.read_bool state.reader

  let deserialize_string _self state = Parser.read_string state.reader

  let deserialize_option self { reader; _ } de =
    print_int 12;
    print_newline ();
    match Parser.peek reader with
    | Some 'n' ->
      let* () = Parser.read_null reader in
      Ok None
    | _ ->
      let* v = De.deserialize self de in
      Ok (Some v)

  let deserialize_identifier self _state visitor =
    print_int 11;
    print_newline ();
    let* str = De.deserialize_string self in
    Visitor.visit_string self visitor str

  (*TODO: read sequence *)
  let deserialize_sequence self s ~size de =
    print_int 10;
    print_newline ();
    s.kind <- First;
    let* v = De.deserialize self (de ~size) in
    Ok v

  (**)
  let deserialize_element self s de =
    print_int 9;
    print_newline ();
    match Parser.peek s.reader with
    | Some ']' -> Ok None
    | _ ->
      let* () =
        if s.kind = First then
          Ok ()
        else
          Ok ()
      in
      s.kind <- Rest;
      let* v = De.deserialize self de in
      Ok (Some v)

  let deserialize_unit_variant _self _state =
    print_int 8;
    print_newline ();
    Ok ()

  let deserialize_newtype_variant self _ de =
    print_int 7;
    print_newline ();
    De.deserialize self de

  let deserialize_tuple_variant self _ ~size de =
    print_int 6;
    print_newline ();
    De.deserialize_sequence self size de

  let deserialize_record_variant self _ ~size de =
    print_int 5;
    print_newline ();
    De.deserialize_record self "" size (de ~size)

  let deserialize_variant self { reader; _ } de ~name:_ ~variants:_ =
    print_int 4;
    print_newline ();
    Parser.skip_row_information reader;
    match Parser.peek reader with
    | Some '{' ->
      let* () = Parser.read_object_start reader in
      Parser.skip_row_information reader;
      let* value = De.deserialize self de in
      Parser.skip_row_information reader;
      let* () = Parser.read_object_end reader in
      Ok value
    | Some '"' -> De.deserialize self de
    | _ -> assert false

  let deserialize_record self s ~name:_ ~size:_ fields =
    print_int 3;
    print_newline ();
    Printf.printf "dagen d : %S" (Bytes.to_string s.reader.value);
    Parser.skip_row_information s.reader;
    match Parser.peek s.reader with
    | Some '{' ->
      let* () = Parser.read_object_start s.reader in
      Parser.skip_row_information s.reader;
      s.kind <- First;
      let* value = De.deserialize self fields in
      Parser.skip_row_information s.reader;
      let* () = Parser.read_object_end s.reader in
      Ok value
    | Some c -> failwith (Printf.sprintf "what: %c" c)
    | None -> failwith "unexpected eof"

  let deserialize_key self s visitor =
    print_int 11;
    print_newline ();
    Parser.skip_row_information s.reader;
    match Parser.peek s.reader with
    | Some '}' -> Ok None
    | _ ->
      let* () =
        if s.kind = First then
          Ok ()
        else
          Parser.read_end s.reader
      in
      s.kind <- Rest;
      Parser.skip_row_information s.reader;
      let* str = De.deserialize_string self in
      Parser.skip_row_information s.reader;
      let* key = Visitor.visit_string self visitor str in
      Parser.skip_row_information s.reader;
      let* () = Parser.read_end s.reader in
      Parser.skip_row_information s.reader;
      Ok (Some key)

  let deserialize_field self s ~name:_ de =
    print_int 2;
    print_newline ();
    Parser.skip_row_information s.reader;
    De.deserialize self de

  let deserialize_ignored_any _self s =
    print_int 1;
    print_newline ();
    Parser.skip_row_information s.reader;
    match Parser.peek s.reader with
    | Some '}' -> Ok ()
    | Some ',' ->
      Parser.skip_row_information s.reader;
      Parser.skip_row_information s.reader;
      Ok ()
    | Some _ ->
      Parser.skip_row_information s.reader;
      Parser.skip_row_information s.reader;
      Ok ()
    | None -> failwith "unexpected eof"
end

let rec parse_row buf de acc =
  if Bytes.get buf 0 = 'D' then
    let state =
      Deserializer.{ reader = Postgres.Parser.of_bytes buf; kind = First }
    in
    let row_length = 120 in
    let* row = Serde.deserialize (module Deserializer) state de in
    parse_row buf de (acc @ [row])
  else
    Ok acc

let of_rows de string = parse_row string de []
