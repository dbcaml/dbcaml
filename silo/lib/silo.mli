open Riot

val start : ?connections:int -> Dbcaml.Driver.t -> (Pid.t, string) result

val fetch_one :
  ?params:Dbcaml.Param.t list option ->
  Pid.t ->
  query:string ->
  deserializer:('a, Serde_postgres.Deserializer.state) Serde.De.t ->
  ('a, string) result
