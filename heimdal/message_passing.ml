open Riot

type 'item new_child = 'item

type Message.t +=
  | CheckIn of Pid.t
  | CheckOut of {
      request_pid: Pid.t;
      child_pid: Pid.t;
    }
  | CheckedOut of Pid.t
  | NewHolder of string

type Message.t += ChildLock of Pid.t | ChildUnlock of Pid.t
