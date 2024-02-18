open Riot

type holder_item = {
  holder_pid: Pid.t;
  item: string;
}

type Message.t +=
  | CheckIn of Pid.t
  | CheckOut of Pid.t
  | LockHolder of Pid.t
  | NewHolder of string
  | HolderMessage of holder_item
