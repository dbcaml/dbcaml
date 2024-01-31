open Riot

type query = string

type Message.t += Query of query

type Message.t += ReadyStatus of string

module Logger = Logger.Make (struct
  let namespace = ["dbcaml"]
end)
(*
   TODO:
     1. Create a function that setups the connection and returns a connection object with PIDs
     2. Create a connection manager
*)
