open Riot

type Message.t +=
  | Result of
      (Row.t list, Execution_result.execution_error) Execution_result.result
  | Query of Query.t
