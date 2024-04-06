open Riot

type Message.t +=
  | CheckIn of Pid.t
  | CheckOut of Pid.t
  | LockHolder of Pid.t
  | NewHolder of Connection.t
  | HolderMessage of Pid.t * Connection.t
