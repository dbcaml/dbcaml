open Riot

type Message.t +=
  | Result of (Row.t list, Result.execution_error) Result.result
  | Query of Query.t
