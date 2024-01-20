open Riot

module Logger = Logger.Make (struct
  let namespace = ["dbcaml"]
end)

module PgPool = Pg_pool.PgPool
