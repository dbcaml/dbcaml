val connection : string -> Dbcaml.Driver.t

module Pg_arguments : sig
  val escape_sql_value : string -> string
end
[@@cfg test]
