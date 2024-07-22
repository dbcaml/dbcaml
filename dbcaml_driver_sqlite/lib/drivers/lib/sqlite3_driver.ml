module Sqlite3 = struct
  type config = { conninfo: string }

  let connect _ = Ok "hello"
end

let connection conninfo =
  Connection.Connection { driver = (module Sqlite3); config = { conninfo } }
