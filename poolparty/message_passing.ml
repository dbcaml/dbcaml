open Riot

type 'item holder_item = 'item

type Message.t +=
  | CheckIn of Pid.t
  | CheckOut of Pid.t
  | LockHolder of Pid.t
  | NewHolder of string
