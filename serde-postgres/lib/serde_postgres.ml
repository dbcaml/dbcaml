open Serde

let ( let* ) = Result.bind

module Postgres = struct
  module Parser = struct
    type t = { lexbuf: Lexing.lexbuf }

    let debug t =
      let buf = Bytes.unsafe_to_string t.lexbuf.lex_buffer in
      let padding = " " ^ String.make t.lexbuf.lex_curr_pos ' ' ^ "^" in
      Printf.printf "buff:\n`%s`\n%s\n" buf padding

    let of_bytes string = { lexbuf = Lexing.from_string string }

    let _run fn = Ok (fn ())

    let peek { lexbuf; _ } =
      if lexbuf.lex_curr_pos < lexbuf.lex_buffer_len then
        Some (Bytes.unsafe_to_string lexbuf.lex_buffer).[lexbuf.lex_curr_pos]
      else
        None

    let read_bool { lexbuf } =
      _run (fun () -> Yojson.Safe.read_bool yojson lexbuf)

    let read_string { lexbuf } =
      _run (fun () -> Yojson.Safe.read_string yojson lexbuf)

    let read_int8 { lexbuf } =
      _run (fun () -> Yojson.Safe.read_int8 yojson lexbuf)

    let read_int { lexbuf } =
      _run (fun () -> Yojson.Safe.read_int yojson lexbuf)

    let read_int32 { lexbuf } =
      _run (fun () -> Yojson.Safe.read_int32 yojson lexbuf)

    let read_int64 { lexbuf } =
      _run (fun () -> Yojson.Safe.read_int64 yojson lexbuf)

    let read_float { lexbuf } =
      _run (fun () -> Yojson.Safe.read_number yojson lexbuf)

    let read_null_if_possible { lexbuf } =
      _run (fun () -> Yojson.Safe.read_null_if_possible yojson lexbuf)

    let read_null { lexbuf } =
      _run (fun () -> Yojson.Safe.read_null yojson lexbuf)

    let read_object_start { lexbuf } =
      _run (fun () -> Yojson.Safe.read_lcurl yojson lexbuf)

    let read_field_sep { lexbuf } =
      _run (fun () -> Yojson.Safe.read_object_sep yojson lexbuf)

    let read_object_end { lexbuf; _ } =
      _run (fun () ->
          try Yojson.Safe.read_object_end lexbuf with
          | Yojson.End_of_object -> ())

    let read_open_bracket { lexbuf } =
      _run (fun () -> Yojson.Safe.read_lbr yojson lexbuf)

    let read_close_bracket { lexbuf } =
      _run (fun () -> Yojson.Safe.read_rbr yojson lexbuf)

    let read_comma { lexbuf } =
      _run (fun () -> Yojson.Safe.read_comma yojson lexbuf)

    let read_colon { lexbuf } =
      _run (fun () -> Yojson.Safe.read_colon yojson lexbuf)

    let skip_space { lexbuf } =
      _run (fun () -> Yojson.Safe.read_space yojson lexbuf) |> ignore;
      ()

    let skip_any { lexbuf } =
      _run (fun () -> Yojson.Safe.skip_json yojson lexbuf) |> ignore;
      ()
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

  let deserialize_sequence self s ~size de =
    let* () = Parser.read_open_bracket s.reader in
    s.kind <- First;
    let* v = De.deserialize self (de ~size) in
    let* () = Parser.read_close_bracket s.reader in
    Ok v

  let deserialize_element self s de =
    match Parser.peek s.reader with
    | Some ']' -> Ok None
    | _ ->
      let* () =
        if s.kind = First then
          Ok ()
        else
          Parser.read_comma s.reader
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
    | Some c -> failwith (Format.sprintf "what: %c" c)
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
