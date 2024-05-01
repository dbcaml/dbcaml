open Riot

type t

module Params = Dbcaml.Params

module type Intf = sig
  val connection : string -> Dbcaml.Driver.t
end

val config : connections:int -> connection_string:string -> t

val connect : config:t -> (Pid.t, [> `Msg of string ]) result

val query :
  ?params:Params.t list ->
  Pid.t ->
  query:string ->
  deserializer:('a, Serde_postgres.Deserializer.state) Serde.De.t ->
  ('a, string) result
