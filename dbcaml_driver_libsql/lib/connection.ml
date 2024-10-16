type t =
  | Connection : {
      conn: 'conn;
      query:
        connection:'conn ->
        params:Dbcaml.Params.t list ->
        query:string ->
        row_limit:int ->
        (bytes, Dbcaml.Res.execution_error) result;
    }
      -> t

let make_connection conn query = Ok (Connection { conn; query })
