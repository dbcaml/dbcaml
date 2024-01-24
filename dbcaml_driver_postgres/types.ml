open Riot

type query = string

type Message.t += Query of query

type Message.t += ReadyStatus of string
