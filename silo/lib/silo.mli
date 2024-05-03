open Riot

type t

module Params = Dbcaml.Params

module type Intf = sig
  val connection : string -> Dbcaml.Driver.t
end

val config :
  connections:int -> driver:(module Intf) -> connection_string:string -> t

val connect : config:t -> (Pid.t, [> `Msg of string ]) result

val query :
  ?params:Params.t list ->
  Pid.t ->
  query:string ->
  deserializer:('a, 'state) Serde.De.t ->
  ('a option, string) result

val parse_command_complete : string -> (int, string) result

val execute :
  ?params:Params.t list -> Pid.t -> query:string -> (int, string) result
