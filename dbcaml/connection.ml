type t =
  | C : {
      conn: 'conn;
      execute: 'conn -> string -> (Row.t list, 'b) Io.io_result;
    }
      -> t

let make ~conn ~execute () = Ok (C { conn; execute })
