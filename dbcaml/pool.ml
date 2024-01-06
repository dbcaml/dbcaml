module PgPool = struct
  type t = {
    max_connections: int;
  }

  let default = {
    max_connections = 10;
  }

  let connect ?(pool=default) ?(host="localhost") ?(port=5432) ?(user="postgres") ?(password="postgres") ?(database="postgres") () =
     (* 
        This function should create a connection pool and return the pool so we can interact with it.
      *)
end

