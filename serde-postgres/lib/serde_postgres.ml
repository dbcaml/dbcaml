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
      _run (fun () -> int_of_string (String.of_bytes value))

    let read_int { value } =
      _run (fun () -> int_of_string (String.of_bytes value))

    let read_int32 { value } =
      _run (fun () -> String.of_bytes value |> Int32.of_string)

    let read_int64 { value } =
      _run (fun () -> String.of_bytes value |> Int64.of_string)

    let read_float { value } =
      _run (fun () -> float_of_string_opt (String.of_bytes value))

    let read_null_if_possible { value } =
      _run (fun () -> Yojson.Safe.read_null_if_possible yojson lexbuf)

    let read_null _ = _run (fun () -> "") (* FIXME: change this *)
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
    match Parser.peek reader with
    | Some 'n' ->
      let* () = Parser.read_null reader in
      Ok None
    | _ ->
      let* v = De.deserialize self de in
      Ok (Some v)

  let deserialize_identifier self _state visitor =
    let* str = De.deserialize_string self in
    Visitor.visit_string self visitor str

  (*TODO: read sequence *)
  let deserialize_sequence self s ~size de =
    s.kind <- First;
    let* v = De.deserialize self (de ~size) in
    Ok v

  (**)
  let deserialize_element self s de =
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

  let deserialize_unit_variant _self _state = Ok ()

  let deserialize_newtype_variant self { reader; _ } de =
    let* () = Parser.read_colon reader in
    De.deserialize self de

  let deserialize_tuple_variant self { reader; _ } ~size de =
    let* () = Parser.read_colon reader in
    De.deserialize_sequence self size de

  let deserialize_record_variant self { reader; _ } ~size de =
    let* () = Parser.read_colon reader in
    De.deserialize_record self "" size (de ~size)

  let deserialize_variant self { reader; _ } de ~name:_ ~variants:_ =
    Parser.skip_space reader;
    match Parser.peek reader with
    | Some '{' ->
      let* () = Parser.read_object_start reader in
      Parser.skip_space reader;
      let* value = De.deserialize self de in
      Parser.skip_space reader;
      let* () = Parser.read_object_end reader in
      Ok value
    | Some '"' -> De.deserialize self de
    | _ -> assert false

  let deserialize_record self s ~name:_ ~size:_ fields =
    Parser.skip_space s.reader;
    match Parser.peek s.reader with
    | Some '{' ->
      let* () = Parser.read_object_start s.reader in
      Parser.skip_space s.reader;
      s.kind <- First;
      let* value = De.deserialize self fields in
      Parser.skip_space s.reader;
      let* () = Parser.read_object_end s.reader in
      Ok value
    | Some c -> failwith (Printf.sprintf "what: %c" c)
    | None -> failwith "unexpected eof"

  let deserialize_key self s visitor =
    Parser.skip_space s.reader;
    match Parser.peek s.reader with
    | Some '}' -> Ok None
    | _ ->
      let* () =
        if s.kind = First then
          Ok ()
        else
          Parser.read_comma s.reader
      in
      s.kind <- Rest;
      Parser.skip_space s.reader;
      let* str = De.deserialize_string self in
      Parser.skip_space s.reader;
      let* key = Visitor.visit_string self visitor str in
      Parser.skip_space s.reader;
      let* () = Parser.read_colon s.reader in
      Parser.skip_space s.reader;
      Ok (Some key)

  let deserialize_field self s ~name:_ de =
    Parser.skip_space s.reader;
    De.deserialize self de

  let deserialize_ignored_any _self s =
    Parser.skip_space s.reader;
    match Parser.peek s.reader with
    | Some '}' -> Ok ()
    | Some ',' ->
      let* _ = Parser.read_comma s.reader in
      Parser.skip_space s.reader;
      let _ = Parser.skip_any s.reader in
      Parser.skip_space s.reader;
      Ok ()
    | Some _ ->
      Parser.skip_space s.reader;
      let _ = Parser.skip_any s.reader in
      Parser.skip_space s.reader;
      Ok ()
    | None -> failwith "unexpected eof"
end

let of_row de string =
  let state =
    Deserializer.{ reader = Postgres.Parser.of_bytes string; kind = First }
  in
  Serde.deserialize (module Deserializer) state de

let of_rows de string =
  let state =
    Deserializer.{ reader = Postgres.Parser.of_bytes string; kind = First }
  in
  Serde.deserialize (module Deserializer) state de
