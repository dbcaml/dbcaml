open Riot

type Message.t +=
  | Result of (Row.t list, Res.execution_error) Res.result
  | Query of Query.t

type holder_item = {
  holder_pid: Pid.t;
  item: Connection.t;
}

type Message.t +=
  | CheckIn of Pid.t
  | CheckOut of Pid.t
  | LockHolder of Pid.t
  | NewHolder of Connection.t
  | HolderMessage of holder_item
