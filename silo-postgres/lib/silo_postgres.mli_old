open Riot

type t

module type Intf = sig
  val connection : string -> Dbcaml.Driver.t
end

val config :
  connections:int -> driver:(module Intf) -> connection_string:string -> t

val connect : config:t -> (Pid.t, string) result

val fetch_many :
  ?params:Dbcaml.Param.t list option ->
  Pid.t ->
  query:string ->
  deserializer:('a, Serde_postgres.Deserializer.state) Serde.De.t ->
  ('a list, string) result
